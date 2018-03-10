FORMAT: 1A

# Flightsayer Flights API

Flightsayer's flights API allows consumers to view the predictive flight status for specific flights (including our delay predictions). Before we outline the API documentation, we start with a few examples of common queries that you may want to make.


1. To obtain flight status for flight `WN55DALHOU1608310100` including historical performance and inbound flight info: 
    * GET `https://api.flightsayer.com/flights/v1/status/WN55DALHOU1608310100?history=true&inbound=true`
2. To retreive flight status for a set of flights (you can specify up to 250, but in this example we have 2): 
    * GET `https://api.flightsayer.com/flights/v1/status?flights=AA3659DFWAEX1610101350,WN1936LASSFO1610131610`
3. To retreive a set of alternate flights when a flight is likely to be delayed:
    * GET `https://api.flightsayer.com/flights/v1/alternates/NK942TPAATL1608182310`
4. To search for all flights that depart between now and two days from now, and whose delay predictions have changed in the last hour (assume it's currently `2016-10-13T15:00Z`: 
    * GET `https://api.flightsayer.com/flights/v1/search?departing_after=2016-10-13T15:00Z&departing_before=2016-10-15T15:00Z&changed_after=2016-10-13T14:00Z`
5. To search for all flights departing in a one hour period between two timestamps: 
    * GET `https://api.flightsayer.com/flights/v1/search?departing_after=2016-10-15T14:00Z&departing_before=2016-10-15T15:00Z`

You can test any of these URLs out with the following curl command, making sure to include your API token in the request header (below we show option `1`):

```
curl -H 'Token: <API token>' https://api.flightsayer.com/flights/v1/status/AA3659DFWAEX1610101350&history=true&inbound=true
```

Note that you'll need to replace the flight IDs and timestamps with valid values (flights expire from our API after they have landed).

All specific details of using this API are below. Contact `info@flightsayer.com` with any questions!


## Retrieve flight status for a flight [GET /flights/v1/status/{flight_id}{?weather,inbound,history}]

Retrieves the status of a specific flight, including delay prediction, and optionally including historical performance, weather, and inbound flight information.

+ Request
    + Parameters
        + flight_id: UA576BOSSFO1606092145 (FlightId, required) - flight id in the form of <IATA carrier code><flight number><departure airport><arrival airport><scheduled departure time as YYMMDDHHMM in UTC time>
        + weather: true (boolean, optional) - set to true to include weather data (default is false)
        + history: true (boolean, optional) - set to true to include historical_performance data (default is false)
        + inbound: true (boolean, optional) - set to true to include inbound flight status, if available (default is false)

    + Headers

            Token: <API token - request from info@flightsayer.com if you do not have one>

+ Response 200 (application/json)

    + Attributes (FlightStatus)
        + flight (object, required) - detailed information for a flight, and uniquely identifies a single leg
            + id (FlightId, required) - flight id of this flight
            + number (number, required) - flight number for this flight
            + carrier (object, required) - carrier information
                + iata (string, required) - the 2-character IATA air carrier code
                + name (string, required) - airline name
            + scheduled_departure (timestamp, required) - scheduled departure time for this flight
            + scheduled_arrival (timestamp, required) - scheduled arrival time for this flight
            + origin (AirportInfo, required) - information about the origin airport
            + destination (AirportInfo, required) - informnation about the departure airport
        + prediction (object, required) - predicted flight delay information
            + delay_index (number, required) -  Value between 0 and 10 representing risk and magnitude of delay, where 10 represents the highest risk of delay. This value is a calculated summary of the delay probability distribution. A value of 1 means that the flight is predicted to be on time, a 10 means a flight is likely to be highly delayed, and values in between indicate increasing levels of delay are predicted. 0 is a special value indicating that either there is not enough historical data for this flight to make a prediction (within 7 days of departure there will always be enough data to make a prediction, so this only applies to farther out predictions) or there is an unusual event that may be affecting the flight. In the latter case, prediction.is_override will be True, and we advise the traveler to check with their airline for further information. Examples of unusual events are an airport evacuation, airline computer glitch, or a hurricane. If the delay index is 0, the `distribution` field should be ignored.
            + distribution (array[number], required) - flightsayer's prediction of delay for this flight, consisting of the following four probabilities:
                + 0 - probability of less than 30 minutes of delay
                + 1 - probabilitiy of between 30 and 60 minutes of delay
                + 2 - probability of between 60 and 120 minutes of delay
                + 3 - probability of 2+ hours of delay
            + risk (FlightsayerRisk, required) - description of risk represented by delay_index
            + causes (array[DelayCause], required) - an array of reasons explaining the cause for the prediction.
            + is_override (boolean, required) - true if the prediction for this flight has delay_index 0 corresponding to a special event. This occurs during extreme events such as airport evacuation or hurricane - see delay_index attribute for more info. Otherwise false.
        + historical_performance (object, optional) - historical on time data for flights that are similar to this (same airline and flight number, same origin and destination, but the exact schedule departure time may vary by an hour).
            + arrival_delay (array[HistoricalDelayValue], required) - An array of 60 values representing the historical arrival performance of this flight over the last 60 days. The first value is 60 days ago and the last value in the array is yesterday.
            + last_updated (timestamp, required) - time at which arrival delay was last updated.
        + status (object, optional) - latest real time flight status
            + text (FlightStatusText, required): description of current status of this flight
            + departure (object, optional): departure status will be available unless flight is cancelled
                + scheduled(timestamp, required) - originally scheduled time of departure
                + latest (timestamp, required) - latest available time of departure
                + type (StatusType, required) - type of status represented by the latest estimate
                + terminal (string, optional) - terminal assignment at the departure airport
                + gate (string, optional) - gate assignment at the departure airport
            + arrival (object, optional): arrival status will be available unless flight is cancelled
                + scheduled(timestamp, required) - originally scheduled time of arrival
                + latest (timestamp, required) - latest available time of arrival
                + type (StatusType, required) - type of status represented by the latest estimate
                + terminal (string, optional) - terminal assignment at the arrival airport
                + gate (string, optional) - gate assignment at the arrival airport
                + baggage_claim (string, optional) - baggage claim assignment
            + diversion (object, optional): included if this flight has been diverted
                + airport (string, optional): IATA airport code for the diversion airport
            + cancelled (boolean, required) - true if the flight has been cancelled
            + source (RealtimeStatusSource, required) - indicates source of the real time data. 
            + last_updated (timestamp, required) - time at which this real time status was most recently updated
        + weather (object, optional) - weather information for the origin and destination airports.
            + origin (WeatherForecast, required) - weather forecast at origin airport at time of departure, or on departure day (as available)
            + destination (WeatherForecast, required) - weather forecast at destination airport at time of arrival, or on arrival day (whichever is available)
        + inbound (FlightStatus, optional) - flight status for the incoming flight

    + Body

            {
              "flight": {
                "id": "VX865DENSFO1704201800",
                "number": 865,
                "carrier": {
                  "iata": "VX",
                  "name": "Virgin America"
                },
                "scheduled_departure": "2017-04-20T12:00:00-06:00",
                "scheduled_arrival": "2017-04-20T13:40:00-07:00",
                "origin": {
                  "iata": "DEN",
                  "city": "Denver",
                  "name": "Denver Intl"
                },
                "destination": {
                  "iata": "SFO",
                  "city": "San Francisco",
                  "name": "San Francisco Intl"
                }
              },
              "prediction": {
                "delay_index": 1,
                "distribution": [
                  0.9,
                  0.1,
                  0,
                  0
                ],
                "risk": "LOW",
                "causes": [
                  "arrival-airport-conditions"
                ],
                "is_override": false,
                "last_changed": "2017-04-20T15:24:37.129Z"
              },
              "historical_performance": {
                "arrival_delay": [-19,10002,10002,8,4,21,21,-8,2,8,-15,-5,-16,-14,3,10,29,77,57,-22,-3,-10,-23,-19,-27,-20,17,-15,-13,-10,46,78,16,72,10002,-4,5,-8,-18,-27,8,-8,-2,48,-10,-14,2,37,171,31,10000,23,-4,77,45,-8,-14,-9,22,73],
                "last_updated": "2017-04-18T18:00:00Z"
              },
              "status": {
                "text": "Scheduled",
                "departure": {
                  "scheduled": "2017-04-20T12:00:00-06:00",
                  "latest": "2017-04-20T12:00:00-06:00",
                  "type": "scheduled",
                  "terminal": "A",
                  "gate": "A39"
                },
                "arrival": {
                  "scheduled": "2017-04-20T13:40:00-07:00",
                  "latest": "2017-04-20T13:36:00-07:00",
                  "type": "estimated",
                  "terminal": "2",
                  "gate": "50B",
                  "baggage_claim": null
                },
                "cancelled": false,
                "source": "flightview",
                "last_updated": "2017-04-20T16:15:00Z"
              },
              "weather": {
                "origin": {
                  "icon": "partly-cloudy-day",
                  "summary": "Mostly Cloudy",
                  "temperature": 62.88,
                  "precipitation": 0,
                  "hourly": true
                },
                "destination": {
                  "icon": "partly-cloudy-day",
                  "summary": "Partly Cloudy",
                  "temperature": 67.36,
                  "precipitation": 0,
                  "hourly": true
                }
              }
            }

+ Response 404 (application/json)
The most common cause of 404s is that the flight ID specified is not of the correct format, or has expired (flight has already landed).

    + Attributes 
        + status_code (number, required)
        + error (string, required)

    + Body

            {
              "status_code": 404,
              "error": "The resource was not found"
            }


## Retrieve flight status for a set of flights [GET /flights/v1/status{?flights,weather,inbound,history}]

Retrieves flight status including delay prediction for upto 250 specific flights, optionally including historical performance, weather, and inbound flight information.

+ Request

    + Parameters
        + flights: `AA3659DFWAEX1610101350,WN1936LASSFO1610131610` (string, required) - comma separated list of at most 250 FlightIds
        + weather: true (boolean, optional) - set to true to include weather data (default is false)
        + history: true (boolean, optional) - set to true to include historical_performance data (default is false)
        + inbound: true (boolean, optional) - set to true to include inbound flight status, if available (default is false)

    + Headers

            Token: thisisasampletoken

+ Response 200 (application/json)

        + array[FlightStatus] - returns flight info for each specified flight.

+ Response 400 (application/json)
The most common cause of 404s is that the flight Ids specified are not of the correct format.

    + Attributes 
        + status_code (number, required)
        + error (string, required)
        
    + Body

            {
              "status_code": 400
              "error": "Bad request: <reason>"
            }


## Get alternative flights for a specific flight [GET /flights/v1/alternates/{flight_id}]

Retrieves alternates flights for the specified flight, sorted by increasing departure time. When the delay index is under 5, no alternates are available.
  
  + Request

    + Parameters
      + flight_id: WN55DALHOU1608300100 (FlightId, required)

    + Headers

            Token: thisisasampletoken

  + Response 200 (application/json)

    + Attributes
      + count (number, required) - number alternates returned
      + flights (array[FlightStatus], required) - array of alternate flights. Flight status includes weather information, but not inbound flight or historical performance. Contact @diana to change what data is included.

    + Body

            {
              "count": 6,
              "flights": [
                {
                  "flight": {
                    "id": "AA2258DFWIAH1608300120", ...
                    } ...
                } ...
                ]
            }


## Search flights [GET /flights/v1/search{?origin,destination,date,codeshare,carrier,flight_number,departing_after,departing_before,changed_after,changed_before,weather}]

Retrieves flight Ids that match a set of filters.

+ Request

    + Parameters
        + origin: BOS (string, optional) - filters by departure airport
        + destination: DEN (string, optional) - filters by arrival airport
        + nearby: true (string, optional) - includes nearby airports when `origin` or `destination` filters are specified
        + date: 2016-06-24 (date, optional) - filter flights that depart on this date (departure airport local time)
        + codeshare: true (boolean, optional) - include codeshare flights - default is false
        + departing_after: 2016-06-24T18:30:00Z (timestamp, optional) - filters flights that depart after the specified time (inclusive)
        + departing_before: 2016-06-24T18:30:00Z (timestamp, optional) - filters flights that depart before the specified time(exclusive)
        + changed_after: 2016-06-24T17:30:00Z (timestamp, optional) - filters flights whose delay predictions were changed after the specified time (inclusive)
        + changed_before: 2016-06-24T17:30:00Z (timestamp, optional) - filters flights whose delay predictions were changed before the specified time (exclusive)
        + carrier: UA (string, optional) - filters by carrier IATA code
        + flight_number: 709 (number, optional) - filters by flight number
        + weather: true (boolean, optional) - if true, includes weather data in with flight info

    + Headers

            Token: thisisasampletoken

+ Response 200 (application/json)

    + Attributes
        + count (number, required) - number of flights matching the filter
        + next (string) - url for next page of search results
        + previous (string) - url for previous page of search results
        + results (array[FlightStatus]) - an array of Flight Statuses. Returns full information regarding a flight

    + Body

            {
              "count": 2672,
              "next": "<url to next page>",
              "previous: null,
              "results": [
                    {
                        "flight": {
                            "id": "AA2586ORDSFO1608292210",
                            "number": 2586,
                            "carrier": {
                            "iata": "AA",
                            "name": "American Airlines"
                            },
                            "scheduled_departure": "2016-08-29T22:10:00-00:05",
                            "scheduled_arrival": "2016-08-30T02:47:00-00:07",
                            "origin": {
                            "iata": "ORD",
                            "city": "Chicago",
                            "name": "Chicago O'Hare Intl"
                            },
                            "destination": {
                            "iata": "SFO",
                            "city": "San Francisco",
                            "name": "San Francisco Intl"
                            }
                        },
                        "prediction": {
                            "delay_index": 3,
                            "distribution": [0.7, 0.1, 0.1, 0.1],
                            "risk": "LOW",
                            "causes": ["arrival-airport-conditions", "late-incoming-flight"],
                            "is_override": false
                        },
                        "status": {
                            "departure": {
                            "scheduled": "2016-08-29T22:10:00-00:05",
                            "latest": "2016-08-29T22:13:00-00:05",
                            "type": "scheduled"
                            },
                            "arrival": {
                            "scheduled": "2016-08-30T02:47:00-00:07",
                            "latest": "2016-08-30T02:11:19-00:07",
                            "type": "scheduled"
                            },
                            "cancelled": false,
                            "source": "swim",
                            "last_updated": "2016-08-28T22:10:43Z"
                        }
                    }
                    // and 2671 more flights
                ]
            }


# Data Structures

## FlightId (string)
A flight id uniquely represents a flight, and takes the form: [IATA carrier code][flight number][departure airport][arrival airport][scheduled departure time as YYMMDDHHMM in UTC time].
For example: `UA576BOSSFO1606092145`

## timestamp (string)
A timestamp in ISO-8601 format, for example: `2016-09-09T15:00:00Z` or `2016-08-25T20:35:00-00:05`. Note that the time zone offset is included for timestamps representing an arrival or departure time, and the offset is the local time at the corresponding origin or destination airport.

## AirportInfo (object)
Information about an airport

+ Attributes

    + iata (string, required) - IATA airport code
    + city (string, required) - airport city name
    + name (string, required) - full airport name

## DelayCause (enum[string])
A reason for the delay prediction. Note that a value of null means that the flight departure is too far out to have a specific cause for delay. Within ~24 hours, delay casues kick in. 
### Members
+ `flight-cancelled` - cancellation source is either swim or flightview
+ `late-incoming-flight` - late inbound flight
+ `arrival-airport-conditions` - ground delay program (GDP) of ground stop (GS) at the scheduled arrival airport, or flight has a controlled time of arrival.
+ `arrival-airport-constraints` - arrival volume constraints: no GDP/GS but high volume (typically happens immediately after a GDP ends)
+ `departure-airport-constraints` - departure constraints: volume exceeds capacity
+ `latest-available-information` - based on latest swim or flightview ETA/ETD. This is a catch all of all airline delays like maintenance.

## HistoricalDelayValue (number)
Represents the number of minutes from the scheduled arrival time (negative number means an early arrival, positive means a late arrival), with special cases in the 10000 range:

- `10000` - no flight (flight was not on the schedule on this day)
- `10001` - no data (flight is in the schedule for this day, but arrival time data is missing)
- `10002` - cancellation (flight was cancelled on this day)
- `10003` - diversion (flight was diverted on this day)

## WeatherForecast (object)
Weather forecast information at origin or destination airport. This forecast corresponds to the weather predicted at the departure or arrival time, respectively.

+ Attributes

    + icon (enum[string], optional) - A summary of weather conditions, suitable for selecting in icon for display. If defined, this property will have one of the following values: `clear-day`, `clear-night`, `rain`, `snow`, `sleet`, `wind`, `fog`, `cloudy`, `partly-cloudy-day`, or `partly-cloudy-night`. Note that additional values such as thunderstorm or tornado may be defined in the future.
    + summary (string, required) - summary of weather conditions
    + temperature(number or array[number], required) - temperature during an hour (for hourly forecast) or array representing low and high temperatures for the day. Note that within about 48 hours out, hourly weather is available and the temperature will be a single value. Beyond this time window, only daily weather is available and the temperature is represented as an array of low/high values. Use the `hourly` field to determine what kind of temperature this is.
    + precipitation (number, required) - probability that it will rain
    + hourly (boolean, required) - true if this object represents an hourly forecast, else it's a daily forecast

## RealtimeStatusSource (enum[string])
Indicates the source of the real time data. The default source of flight data is `flightview`. To obtain `swim` data, contact us at `info@flightsayer.com`.
### Members
+ `flightview` - real time data comes from [flightview's](http://www.flightview.com/) real time flight status feed. This is the default.
+ `schedule` - status comes from the flight schedule - no real time data available. This is commonly seen hours ahead of operations before real time data has kicked in.
+ `swim` - real time data comes from FAA's SWIM data service. Contact us if you're interested in SWIM data.

## FlightStatusText (enum[string])
Indicates the latest real time status of the flight.
### Members
+ `Scheduled` - flight is scheduled to depart, and no other real time data has been made available by the FAA or airline.
+ `Departed` - flight has left the departure gate, but has not taken off
+ `Enroute` - flight is in the air (in many cases the wheels up time is delayed 5 minutes in accordance with FAA regulations.)
+ `Landed` - flight has landed
+ `Arrived` - flight has arrived at the arrival gate
+ `Expected` - the flight is expected to land at the arrival airport soon
+ `Delayed` - the flight has been delayed by the airline or the FAA.
+ `Cancelled` - the flight has been cancelled
+ `Diverted` - the flight has been diverted
+ `No Recent Info - Call Airline` - it is past the departure window for this flight, but there is no updated information.

## StatusType (enum[string])
The status associated with as estimated arrival or departure time
### Members
+ `scheduled` - originally scheduled departure time
+ `estimated` - departure time is estimated based on latest traffic and airline estimates
+ `actual` - actual time (flight has already departed or arrived)

## FlightsayerRisk (enum[string])
A risk name associated with a delay index
### Members
+ `LOW`
+ `MODERATE`
+ `HIGH`
+ `SEVERE`
+ `UNKNOWN` - associated with delay_score of 0, indicating a special circumstance

## FlightStatus (object)
