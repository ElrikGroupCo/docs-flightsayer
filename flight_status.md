FORMAT: 1A

# Flightsayer Flights API

Flightsayer's flights API allows consumers to view the flight status for specific flights.

The api lives at `https://api.flightsayer.com`, so to obtain flightstatus for flight `9K1037BOSSLK1606102040`, do:

```
#!curl


curl -v https://api.flightsayer.com/flights/v1/status/9K1037BOSSLK1606102040?history=true&inbound=true -H 'Authorization: Token <insert token>'
```

## Retrieve flight status [GET /flights/v1/status/{flight_id}{?weather,inbound,history}]

Retrieves the status of a specific flight. 

+ Request
    + Parameters
        + flight_id: UA576BOSSFO1606092145 (FlightId, required) - flight id in the form of <IATA carrier code><flight number><departure airport><arrival airport><scheduled departure time as YYMMDDHHMM in UTC time>
        + weather: true (boolean, optional) - set to true to include weather data (default is false)
        + history: true (boolean, optional) - set to true to include historical_performance data (default is false)
        + inbound: true (boolean, optional) - set to true to include inbound flight status, if available (default is false)

    + Headers

            Authorization: Token sdfiux

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
            + delay_index (number, required) - Flightsayer delay index: a value between 1 and 10, representing a combination of the predicted delay. 1 means the flight is predicted to be ontime, and 10 means a flight is likely to be highly delayed.
            + distribution (array[number], required) - flightsayer's prediction of delay for this flight, consisting of the following four probabilities:
                + distribution[0] - probability of less than 30 minutes of delay
                + distribution[1] - probabilitiy of between 30 and 60 minutes of delay
                + distribution[2] - probability of between 60 and 120 minutes of delay
                + distribution[3] - probability of 2+ hours of delay
            + causes (array[DelayCause], required) - an array of reasons explaining the cause for the prediction.
        + historical_performance (object, optional) - historical on-time data for flights that are similar to this (same airline and flight number, same origin and destination, but the exact schedule departure time may vary by an hour).
                + arrival_delay (array[HistoricalDelayValue], required) - An array of 60 values representing the historical arrival performance of this flight over the last 60 days. The first value is 60 days ago and the last value in the array is yesterday.
                + last_updated (timestamp, required) - time at which arrival_delay was last updated. Note: Currently null for all cases, expect to go live shortly.
        + status (object, optional) - latest real time flight status
            + departure (object, required):
                + scheduled(timestamp, required) - scheduled time of departure
                + latest (timestamp, required) - latest estimated time of departure
                + type (EstimatedDepartureStatus, required) - type of status represented by the latest estimate
            + arrival (object, required):
                + scheduled(timestamp, required) - scheduled time of arrival
                + latest (timestamp, required) - latest estimated time of arrival
                + type (EstimatedArrivalStatus, required) - type of status represented by the latest estimate
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
                "causes": ["arrival-airport-conditions", "late-incoming-flight"]
              },
              "historical_performance": {
                "arrival_delay": [26,14,-8,-19,-9,-27,10,61,101,14,-11,164,-3,-10,105,163,124,6,261,8,40,0,4,-16,-16,-15,-14,-10,-15,10002,87,-16,-19,-23,56,6,34,41,5,-7,-10,-5,13,-9,27,-26,-13,-1,-14,-14,42,0,131,74,-15,-11,1,1,-9,-13],
                "last_updated": null
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
              },
              "inbound": {
                "flight": {
                  "id": "AA1409MSPORD1608291932",
                  "number": 1409,
                  "carrier": {
                    "iata": "AA",
                    "name": "American Airlines"
                  },
                  "scheduled_departure": "2016-08-29T19:32:00-00:05",
                  "scheduled_arrival": "2016-08-29T21:00:00-00:05",
                  "origin": {
                    "iata": "MSP",
                    "city": "Minneapolis",
                    "name": "Minneapolis-St Paul Intl/Wold-Chamberlain"
                  },
                  "destination": {
                    "iata": "ORD",
                    "city": "Chicago",
                    "name": "Chicago O'Hare Intl"
                  }
                },
                "prediction": {
                  "delay_index": 3,
                  "distribution": [0.7,0.1,0.1,0.1],
                  "causes": ["arrival-airport-conditions"]
                },
                "historical_performance": {
                  "arrival_delay": [10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,-9,19,6,-9,-7],
                  "last_updated": null
                },
                "status": {
                  "departure": {
                    "scheduled": "2016-08-29T19:32:00-00:05",
                    "latest": "2016-08-29T19:41:00-00:05",
                    "type": "scheduled"
                  },
                  "arrival": {
                    "scheduled": "2016-08-29T21:00:00-00:05",
                    "latest": "2016-08-29T20:36:08-00:05",
                    "type": "scheduled"
                  },
                  "cancelled": false,
                  "source": "swim",
                  "last_updated": "2016-08-28T19:35:38Z"
                }
              }
            }

## Retrieve flight status for a filtered set of flights [GET /flights/v1/search/{?departure_airport,arrival_airport,earliest_departure,latest_departure,history,inbound,weather}]

Retrieves flight status for a filtered set of flights.

+ Request

    + Parameters
        + departure_airport: BOS (string, optional) - filters by departure airport
        + arrival_airport: DEN (string, optional) - filters by arrival airport
        + earliest_departure: 2016-06-24T18:30:00Z (timestamp, optional) - filters flights by minumum scheduled departure time (inclusive)
        + latest_departure: 2016-06-24T18:30:00Z (timestamp, optional) - filters flights by maximum scheduled departure time (inclusive)
        + history: true (boolean, optional) - include historical_performance data in response (default is false)
        + inbound: true (boolean, optional) - include inbound flight status in response, if available (default is false)
        + weather: true (boolean, optional) - include weather data in response (default is false)

    + Headers

            Authorization: Token sdfiux

+ Response 200 (application/json)

    + Attributes
        + count (number, required) - number of flights matching the filter
        + next (string, optional) - url pointing to the next set of paginated results
        + previous (string, optional) - url pointing to the previous set of paginated results
        + results (array[FlightStatus]) - an array of FlightStatus objects

    + Body

            {
                "count": 297,
                "next": "https://api.flightsayer.com/flights/v1/search/?limit=50&earliest_departure=2016-06-24T18%3A30%3A00Z&latest_departure=2016-06-24T18%3A30%3A00Z&offset=50",
                "previous": null,
                "results": [
                    {
                        "flight": {
                            ...
                        },
                    },
                    // plus 296 more FlightStatus results
                ]
            }


# Data Structures

## FlightId (string)
A flight id uniquely represents a flight, and takes the form: <IATA carrier code><flight number><departure airport><arrival airport><scheduled departure time as YYMMDDHHMM in UTC time>.
For example: UA576BOSSFO1606092145

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
Weather forecast information at origin or destination airport

+ Attributes

    + summary (string, required) - summary of weather conditions
    + temperature(number or array[number], required) - temperature during an hour (for hourly forecast) or array representing low and high temperatures for the day
    + precipitation (number, required) - probability that it will rain
    + hourly (boolean, required) - true if this object represents an hourly forecast, else it's a daily forecast

## RealtimeStatusSource (enum[string])
Indicates the source of the real time data
### Members
+ `swim` - real time data comes from FAA's SWIM data service

## EstimatedDepartureStatus (enum[string])
The status associated with as estimated departure time
### Members
+ `scheduled` - originally scheduled departure time
+ `proposed` - the airline has proposed a change to the depature time, but faa has not yet accepted 
+ `estimated` - departure time is estimated based on latest traffic and airline estimates
+ `actual` - actual departure time

## EstimatedArrivalStatus (enum[string])
The status associated with as estimated arrival time
### Members 
+ `scheduled` - originally scheduled arrival time
+ `estimated` - arrival time is estimated based on latest traffic and airline estimates
+ `actual` - actual arrival time

## FlightStatus (object)
