FORMAT: 1A

# Flightsayer Flights API

Flightsayer's flights API allows consumers to view the flight status for specific flights.

The api lives at `https://api.flightsayer.com`, so to obtain flightstatus for flight `9K1037BOSSLK1606102040`, do:

```
#!curl


curl -v https://api.flightsayer.com/flights/v1/status/9K1037BOSSLK1606102040 -H 'Authorization: Token <insert token>'
```

## Retrieve flight status [GET /flights/v1/status/{flight_id}]

Retrieves the status of a specific flight. 

+ Parameters
    + flight_id: UA576BOSSFO1606092145 (FlightId, required) - flight id in the form of <IATA carrier code><flight number><departure airport><arrival airport><scheduled departure time as YYMMDDHHMM in UTC time>

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
            + prediction (array[number], required) - flightsayer's prediction of delay for this flight, consisting of the following four probabilities:
                + delay_prediction[0] - probability of less than 30 minutes of delay
                + delay_prediction[1] - probabilitiy of between 30 and 60 minutes of delay
                + delay_prediction[2] - probability of between 60 and 120 minutes of delay
                + delay_prediction[3] - probability of 2+ hours of delay
            + reasons (array[string], required) - an array of reasons explaining the cause for the prediction. This is currently a natural language sentence.
        + historical_performance (object, optional) - historical on-time data for flights similar to this (same airline and flight number, same origin and destination, but the exact schedule departure time may vary by an hour).
            + samples (number, required) - number of times the flight was flown in the last n_months
            + n_months (number, required) - number of months over which historical data has been collected
            + on_time_percentage (number, required) - percentage of time this flight has been on time over the samples
            + cancel_percentage (number, required) - percentage of time this flight has been cancelled over the samples
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
        + inbound (FlightStatus, optional) - flight status for the incoming flight

    + Body

            {
              "flight": {
                "id": "DL5267GTRATL1608252104",
                "number": 5267,
                "carrier": {
                  "iata": "DL",
                  "name": "Delta Air Lines"
                },
                "scheduled_departure": "2016-08-25T21:04:00-00:05",
                "scheduled_arrival": "2016-08-25T22:19:00-00:04",
                "origin": {
                  "iata": "GTR",
                  "city": "Columbus/W Point/Starkville",
                  "name": "Golden Triangle Rgnl"
                },
                "destination": {
                  "iata": "ATL",
                  "city": "Atlanta",
                  "name": "Hartsfield - Jackson Atlanta Intl"
                }
              },
              "prediction": {
                "delay_index": 2,
                "distribution": [
                  0.8,
                  0.1,
                  0.1,
                  0
                ]
              },
              "historical_performance": null,
              "status": {
                "departure": {
                  "scheduled": "2016-08-25T21:04:00-00:05",
                  "latest": "2016-08-25T21:08:00-00:05",
                  "type": "scheduled"
                },
                "arrival": {
                  "scheduled": "2016-08-25T22:19:00-00:04",
                  "latest": "2016-08-25T21:45:59-00:04",
                  "type": "scheduled"
                },
                "cancelled": false,
                "source": "swim",
                "last_updated": "2016-08-25T14:28:10Z"
              },
              "weather": {
                "origin": {
                  "summary": "Drizzle",
                  "temperature": 96.31,
                  "precipitation": 0.38,
                  "hourly": true
                },
                "destination": {
                  "summary": "Mostly Cloudy",
                  "temperature": 90.86,
                  "precipitation": 0,
                  "hourly": true
                }
              },
              "inbound": {
                "flight": {
                  "id": "DL5267ATLGTR1608251930",
                  "number": 5267,
                  "carrier": {
                    "iata": "DL",
                    "name": "Delta Air Lines",
                    "friendly_name": "Delta"
                  },
                  "scheduled_departure": "2016-08-25T19:30:00-00:04",
                  "scheduled_arrival": "2016-08-25T20:35:00-00:05",
                  "origin": {
                    "iata": "ATL",
                    "city": "Atlanta",
                    "name": "Hartsfield - Jackson Atlanta Intl"
                  },
                  "destination": {
                    "iata": "GTR",
                    "city": "Columbus/W Point/Starkville",
                    "name": "Golden Triangle Rgnl"
                  }
                },
                "prediction": {
                  "delay_index": 1,
                  "distribution": [
                    1,
                    0,
                    0,
                    0
                  ]
                },
                "historical_performance": null,
                "status": {
                  "departure": {
                    "scheduled": "2016-08-25T19:30:00-00:04",
                    "latest": "2016-08-25T19:37:00-00:04",
                    "type": "scheduled"
                  },
                  "arrival": {
                    "scheduled": "2016-08-25T20:35:00-00:05",
                    "latest": "2016-08-25T20:19:33-00:05",
                    "type": "scheduled"
                  },
                  "cancelled": false,
                  "source": "swim",
                  "last_updated": "2016-08-25T14:28:10Z"
                },
                "weather": {
                  "origin": {
                    "summary": "Partly Cloudy",
                    "temperature": 90.97,
                    "precipitation": 0,
                    "hourly": true
                  },
                  "destination": {
                    "summary": "Drizzle",
                    "temperature": 97.06,
                    "precipitation": 0.25,
                    "hourly": true
                  }
                }
              }
            }

## Retrieve flight status for a filtered set of flights [GET /flights/v1/search{departure_airport,arrival_airport,earliest_departure,latest_departure}]

Retrieves flight status for a filtered set of flights.

+ Parameters
    + departure_airport: BOS (string, optional) - filters by departure airport
    + arrival_airport: DEN (string, optional) - filters by arrival airport
    + earliest_departure: 2016-06-24T18:30:00Z (timestamp, optional) - filters flights by minumum scheduled departure time (inclusive)
    + latest_departure: 2016-06-24T18:30:00Z (timestamp, optional) - filters flights by maximum scheduled departure time (inclusive)

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
