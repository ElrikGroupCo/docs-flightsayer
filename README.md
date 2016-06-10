FORMAT: 1A

# Flight status

Flightsayer's flights API allows consumers to view the flight status for specific flights. 

The api lives at api.flightsayer.com, so to obtain flightstatus for flight UA576BOSSFO1606092145, do:

```
#!curl


curl -v http://api.flightsayer.com/flights/v1/status/BA5720SFODFW1606091908/ -H 'Authorization: Token <insert token>'
```


## Retreive flight status [GET /flights/v1/status/{flight_id}]

Retreives the status of a specific flight. 

+ Parameters
    + flight_id: UA576BOSSFO1606092145 (required, string) - flight id in the form of <IATA carrier code><flight number><departure airport><arrival airport><scheduled departure time as YYMMDDHHMM>

+ Response 200 (application/json)

    + Attributes (FlightStatus)
        + id (string, required) - flight id
        + flight_info (object, required) - detailed information for a flight, and uniquely identifies a single leg
            + master_flight_id (string, required) - flight id if the master flight. This is different from the flight id of the requested flight status object in the case of code shares.
            + carrier_code_iata (string, required) - the 2-character IATA air carrier code
            + flight_number (number, required) - flight number for this flight
            + departure_airport (string, required) - departure airport for this flight
            + arrival_airport (string, required) - arrival airport for this flight
            + scheduled_departure (string, required) - scheduled departure time for this flight, in iso-8601 format
            + scheduled_arrival (string, required) - scheduled arrival time for this flight, in iso-8601 format
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
            + estimated_departure (string, required) - estimated time of departure, in iso-8601 format
            + estimated_departure_status (RealtimeStatus, required)
            + estimated_arrival (string, required) - estimated time of arrival, in iso-8601 format
            + estimated_arrival_status (RealtimeStatus, required)
            + cancelled (boolean, required) - true if the flight has been cancelled
            + last_updated (string, required) - time at which this real time status was most recently updated, in iso-8601 format
            + source (enum[string], required) - indicates source of the real time data. 
                + Members
                    + `swim`
                    + `flightview`
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
                    "estimated_departure": "2016-06-10T01:06:00Z",
                    "estimated_departure_status": "proposed",
                    "estimated_arrival": "2016-06-10T06:56:00Z",
                    "estimated_arrival_status": "estimated",
                    "cancelled": false,
                    "last_updated": "2016-06-09T20:13:53Z",
                    "source": "swim"
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
                        "estimated_departure": "2016-06-09T16:50:00Z",
                        "estimated_departure_status": "actual",
                        "estimated_arrival": "2016-06-09T21:49:45Z",
                        "estimated_arrival_status": "estimated",
                        "cancelled": false,
                        "last_updated": "2016-06-09T16:50:19Z",
                        "source": "swim"
                    }
                },
                "incoming_confirmed": false
            }

# Data Structures

## RealtimeStatus (enum[string])
Indicates the arrival or departure status of a flight
### Members 
+ `estimated`
+ `actual`

## FlightStatus (object)