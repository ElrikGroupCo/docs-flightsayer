FORMAT: 1A

# Flightsayer Flights API

Flightsayer's flights API allows consumers to view the flight status for specific flights. 

The api lives at `api.flightsayer.com`, so to obtain flightstatus for flight `9K1037BOSSLK1606102040`, do:

```
#!curl


curl -v https://api.flightsayer.com/flights/v1/status/9K1037BOSSLK1606102040/ -H 'Authorization: Token <insert token>'
```

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
                        "id": "UA132DFWIAH1606241830",
                        "flight_info": {
                            "master_flight_id": "UA132DFWIAH1606241830",
                            "carrier_code_iata": "UA",
                            "flight_number": 132,
                            "departure_airport": "DFW",
                            "arrival_airport": "IAH",
                            "scheduled_departure": "2016-06-24T18:30:00Z",
                            "scheduled_arrival": "2016-06-24T19:42:00Z"
                        },
                        // plus 296 more FlightStatus results
                    }
                ]
            }

## Retrieve flight status [GET /flights/v1/status/{flight_id}]

Retrieves the status of a specific flight. 

+ Parameters
    + flight_id: UA576BOSSFO1606092145 (string, required) - flight id in the form of <IATA carrier code><flight number><departure airport><arrival airport><scheduled departure time as YYMMDDHHMM in UTC time>

+ Response 200 (application/json)

    + Attributes (FlightStatus)
        + id (string, required) - flight id
        + flight_info (object, required) - detailed information for a flight, and uniquely identifies a single leg
            + master_flight_id (string, required) - flight id if the master flight. This is different from the flight id of the requested flight status object in the case of code shares.
            + carrier_code_iata (string, required) - the 2-character IATA air carrier code
            + flight_number (number, required) - flight number for this flight
            + departure_airport (string, required) - departure airport for this flight
            + arrival_airport (string, required) - arrival airport for this flight
            + scheduled_departure (timestamp, required) - scheduled departure time for this flight
            + scheduled_arrival (timestamp, required) - scheduled arrival time for this flight
        + departure_airport_weather (string, optional) - forecast weather at the destination airport at the scheduled time of departure
        + arrival_airport_weather (string, optional) - forecast weather at the arrival airport at the scheduled time of arrival
        + delay_prediction (array[number], required) - flightsayer's prediction of delay for this flight, consisting of the following four probabilities:
            + delay_prediction[0] - probability of less than 30 minutes of delay
            + delay_prediction[1] - probabilitiy of between 30 and 60 minutes of delay
            + delay_prediction[2] - probability of between 60 and 120 minutes of delay
            + delay_prediction[3] - probability of 2+ hours of delay
        + historical_ontime_performance (object, optional) - historical on-time data for flights similar to this (same airline and flight number, same origin and destination, but the exact schedule departure time may vary by an hour).
            + samples (number, required) - number of times the flight was flown in the last n_months
            + n_months (number, required) - number of months over which historical data has been collected
            + on_time_percentage (number, required) - percentage of time this flight has been on time over the samples
            + cancel_percentage (number, required) - percentage of time this flight has been cancelled over the samples
        + realtime_status (object, optional) - latest real time flight status
            + source (RealtimeStatusSource, required) - indicates source of the real time data. 
            + last_updated (timestamp, required) - time at which this real time status was most recently updated
            + estimated_departure (timestamp, required) - estimated time of departure
            + estimated_departure_status (EstimatedDepartureStatus, required)
            + estimated_arrival (timestamp, required) - estimated time of arrival
            + estimated_arrival_status (EstimatedArrivalStatus, required)
            + cancelled (boolean, required) - true if the flight has been cancelled
            + departure_terminal (string, optional)
            + departure_gate (string, optional)
            + baggage_claim (string, optional)
        + incoming_flight (FlightStatus, optional) - flight status for the incoming flight
        + incoming_confirmed (boolean, required) - true if the incoming flight id is the flight verified by the airline, false if its based on flightsayer's internal prediction

    + Body

            {
                "id": "UA576BOSSFO1606092145",
                "flight_info": {
                    "master_flight_id": "UA576BOSSFO1606092145",
                    "carrier_code_iata": "UA",
                    "flight_number": 576,
                    "departure_airport": "BOS",
                    "arrival_airport": "SFO",
                    "scheduled_departure": "2016-06-09T21:45:00Z",
                    "scheduled_arrival": "2016-06-10T04:12:00Z"
                },
                "departure_airport_weather": "mostly cloudy",
                "arrival_airport_weather": "partly cloudy",
                "delay_prediction": [
                    0.2,
                    0.0,
                    0.1,
                    0.8
                ],
                "historical_ontime_performance": {
                    "samples": 34,
                    "n_months": 2,
                    "on_time_percentage": 76.0,
                    "cancel_percentage": 0.0
                },
                "realtime_status": {
                    "source": "swim"    
                    "last_updated": "2016-06-09T20:13:53Z",
                    "estimated_departure": "2016-06-10T01:06:00Z",
                    "estimated_departure_status": "proposed",
                    "estimated_arrival": "2016-06-10T06:56:00Z",
                    "estimated_arrival_status": "estimated",
                    "cancelled": false,
                },
                "incoming_flight": {
                    "id": "UA768SFOBOS1606091500",
                    "flight_info": {
                        "master_flight_id": "UA768SFOBOS1606091500",
                        "carrier_code_iata": "UA",
                        "flight_number": 768,
                        "departure_airport": "SFO",
                        "arrival_airport": "BOS",
                        "scheduled_departure": "2016-06-09T15:00:00Z",
                        "scheduled_arrival": "2016-06-09T20:35:00Z"
                    },
                    "departure_airport_weather": "overcast",
                    "arrival_airport_weather": "partly cloudy",
                    "delay_prediction": [
                        1.0,
                        0.0,
                        0.0,
                        0.0
                    ],
                    "historical_ontime_performance": {
                        "samples": 59,
                        "n_months": 2,
                        "on_time_percentage": 75.0,
                        "cancel_percentage": 0.0
                    },
                    "realtime_status": {
                        "source": "swim",    
                        "last_updated": "2016-06-09T16:50:19Z",
                        "estimated_departure": "2016-06-09T16:50:00Z",
                        "estimated_departure_status": "actual",
                        "estimated_arrival": "2016-06-09T21:49:45Z",
                        "estimated_arrival_status": "estimated",
                        "cancelled": false,
                    }
                },
                "incoming_confirmed": false
            }

# Data Structures

## timestamp (string)
A timestamp in ISO-8601 format, for example: `2016-09-09T15:00:00Z`. All timestamps in the API are UTC time.

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