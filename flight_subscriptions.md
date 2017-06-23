FORMAT: 1A

# Flightsayer Flight Subscriptions API

Flightsayer's flights API allows consumers to subscribe to push notifications so that they are automatically notified when flight status (including delay prediction) changes.

The api lives at `https://api.flightsayer.com/flights/v1`. 

### Quick start 
To track flight United 1122 on 5/29/17 (flight id `UA1122DENBOS1705291425`) and receive push notifications at `http://target-url.com` (this is your server):
```
curl -X POST -H 'Token: <api-token>' -H 'Content-Type: application/json' \
    -d '{"flight_ids": ["UA1122DENBOS1705291425"], "target": "http://target-url.com"}' \
    https://api.flightsayer.com/flights/v1/subscriptions/
```
where `api-token` is the API token you have been provided (contact `info@flightsayer.com` if you need an API token).

To see the data that is pushed to the callback URL provided, check out `FlightStatusUpdate` in the `Data Structures` section.

### For info on Check out the pull version of the flight status API [here](flight_status.md)
 
# Subscriptions [/subscriptions]

The subscriptions endpoint allows customers to subscribe for push notifications for flights, and to manage their existing notifications.

### Retrieve all subscriptions [GET /subscriptions/]
Retrieve all current flight subscriptions

+ Request (application/json)

    + Headers

            Token: thisisasampletoken

+ Response 200 (application/json)

        + Attributes
            + count (number, required) - number of flights matching the filter
            + next (string, optional) - url pointing to the next set of paginated results
            + previous (string, optional) - url pointing to the previous set of paginated results
            + results (array[FlightSubscription]) - an array of FlightSubscription objects

        + Body
                {
                  "count": 1,
                  "next": null,
                  "previous": null,
                  "results": [
                    {
                      "itinerary_id": "9bb348b3-d567-4392-818f-e19b7ad7fbf2",
                      "flight_ids": ["AS22SEAORD1705021910", "B6112ORDBOS1705030035"],
                      "target": "http://requestb.in/wr9k2hwr",
                      "booking_reference": "78XNKDS",
                      "created": "2017-05-02T16:26:02.883037",
                      "updated": "2017-05-02T16:26:02.883060"
                    }
                  ]
                }

### Create a subscription for the specified itinerary [POST /subscriptions/]
To subscribe to push notifications for an itinerary, first obtain valid flight IDs for the flight and a target URL. Check out `FlightStatusUpdate` in the `Data Structures` section for information about what the data you will then receive via callback.

+ Request (application/json)

    + Headers
    
            Token: thisisasampletoken
            
    + Attributes
     
        + flight_ids (array[FlightId], required) - an array of flight IDs that represent a single itinerary
        + target (string, required) - url to which POST requests indicating change to flight status will be sent.
        + booking_reference (string, optional) - information to attach to this flight such as PNR number.

    + Body

            {
                "flight_ids": ["B6112ORDBOS1705030035", "AS22SEAORD1705021910"]
                "target": "http://status.concernedpassenger.com",
                "booking_reference: "78XNKDL"
            }

+ Response 201 (application/json)
New subscription created.

    + Attributes (FlightSubscription)

    + Body

            {
                "itinerary_id": "9bb348b3-d567-4392-818f-e19b7ad7fbf2",
                "flight_ids": ["AS22SEAORD1705021910", "B6112ORDBOS1705030035"],
                "target": "http://requestb.in/wr9k2hwr",
                "booking_reference": "78XNKDL",
                "created": "2017-05-02T16:26:02.883037",
                "updated": "2017-05-02T16:26:02.883060"
            }

### Retrieve a subscription [GET /subscriptions/{itinerary_id}]
Retrieve a subscription for a specific itinerary

+ Request (application/json)

    + Parameters
        + itinerary_id: 9bb348b3-d567-4392-818f-e19b7ad7fbf2 (string, required) - uuid representing an itinerary

    + Headers
    
            Token: thisisasampletoken

+ Response 200 (application/json)

    + Attributes (FlightSubscription)
    
    + Body
    
                {
                    "itinerary_id": "cabb3527-3c77-488e-8306-f8bf38a904ef",
                    "flight_ids": ["UA619SEAORD1705030620"],
                    "target": "http://requestb.in/wr9k2hwr",
                    "booking_reference": "78XNKDS",
                    "created": "2017-05-02T16:42:17.650792",
                    "updated": "2017-05-02T16:42:17.650812"
                }


### Delete a subscription [DELETE /subscriptions/{itinerary_id}]
Delete the subscription for the specified itinerary.

+ Request (application/json)

    + Parameters
        + itinerary_id: 9bb348b3-d567-4392-818f-e19b7ad7fbf2 (string, required) - uuid representing an itinerary

    + Headers
    
            Token: thisisasampletoken
            
+ Response 204 (application/json)

# Data Structures

## FlightId (string)
A flight id uniquely represents a flight, and takes the form: [IATA carrier code][flight number][departure airport][arrival airport][scheduled departure time as YYMMDDHHMM in UTC time].
For example: `UA576BOSSFO1606092145`

## FlightSubscription (object)
A subscription for flight status alerts
+ Attributes
    + itinerary_id (string, required) - uuid such as `cabb3527-3c77-488e-8306-f8bf38a904ef` identifying the itinerary
    + flight_ids (array[FlightId], required) - array of flight IDs representing the flights in this itinerary
    + target (string, required) - target URL where updates to flight status are sent as a POST request
    + booking_reference (string, optional) - additional information to be stored along with this itinerary, such as PNR
    + created (timestamp, required) - timestamp at which the subscription was created
    + updated (timestamp, required) - timestamp at which the subscription was last updated

## FlightStatusUpdate (object)
When subscribed to an itinerary, this is the format received at the call back URL provided.
  
+ Attributes
    + alert (object, required) - information about why this notification is being sent
        + itinerary_id: `2b68dfef-67c1-46db-8789-4d5d82f14838` (string, required) - this itinerary id corresponding to this push notification
        + booking_reference: `78XNKDS` (string, optional) - optional string such as PNR that references this itinerary
        + change: `initial-status` (PushNotificationChange, required) - the reason this notification is being sent
    + flight (object, required) - detailed information for a flight, and uniquely identifies a single leg
        + id: `DL2009ORDATL1705082330` (FlightId, required) - flight id of this flight
        + number: `2009` (number, required) - flight number for this flight
        + carrier (object, required) - carrier information
            + iata: `DL` (string, required) - the 2-character IATA air carrier code
            + name: `Delta Air Lines` (string, required) - airline name
        + scheduled_departure: `2017-05-08T18:30:00-05:00` (timestamp, required) - scheduled departure time for this flight
        + scheduled_arrival: `2017-05-08T21:40:00-04:00` (timestamp, required) - scheduled arrival time for this flight
        + origin (AirportInfo, required) - information about the origin airport
        + destination (AirportInfo, required) - informnation about the departure airport
    + prediction (object, required) - predicted flight delay information
        + delay_index: `2` (number, required) - Flightsayer delay index: a value between 0 and 10, representing a combination of the predicted delay. 1 means the flight is predicted to be ontime, and 10 means a flight is likely to be highly delayed, and values inbetween indicate increasing levels of delay are predicted. A value is 0 is a special value indicating that unusual events are affecting the flight, and we advise the flyer to check with their airline for further information. Examples of unusual events are an airport evacuation, airline computer glitch, or a gigantic hurricane. If the delay index is 0, the `distribution` field should not be used.
        + distribution (array[number], required) - flightsayer's prediction of delay for this flight, consisting of the following four probabilities:
            + 0: `0.85` - probability of less than 30 minutes of delay
            + 1: `0.1` - probabilitiy of between 30 and 60 minutes of delay
            + 2: `0.05` - probability of between 60 and 120 minutes of delay
            + 3: `0` - probability of 2+ hours of delay
        + risk: `LOW` (FlightsayerRisk, required) - description of risk represented by delay_index
        + causes (array[DelayCause], required) - an array of reasons explaining the cause for the prediction.
        + connections (array[ConnectionInfo], optional) - if this itinerary contains more than one flight, this shows the risk of missing a connection in the itinerary. Each connection involves an airport, and the probability of missing a connection at that airport is listed.
    + status (object, optional) - latest real time flight status
        + text: `Scheduled` (FlightStatusText, required): description of current status of this flight
        + departure (object, optional): departure status will be available unless flight is cancelled
            + scheduled: `2017-05-08T18:30:00-05:00` (timestamp, required) - originally scheduled time of departure
            + latest: `2017-05-08T18:30:00-05:00` (timestamp, required) - latest available time of departure
            + type: `scheduled` (StatusType, required) - type of status represented by the latest estimate
            + terminal: `2` (string, optional) - terminal assignment at the departure airport
            + gate: `E14` (string, optional) - gate assignment at the departure airport
        + arrival (object, optional): arrival status will be available unless flight is cancelled
            + scheduled: `2017-05-08T21:40:00-04:00` (timestamp, required) - originally scheduled time of arrival
            + latest: `2017-05-08T21:39:00-04:00` (timestamp, required) - latest available time of arrival
            + type: `scheduled` (StatusType, required) - type of status represented by the latest estimate
            + terminal: `S` (string, optional) - terminal assignment at the arrival airport
            + gate: `D09` (string, optional) - gate assignment at the arrival airport
            + baggage_claim: `1` (string, optional) - baggage claim assignment. null means not yet available.
        + cancelled: `false` (boolean, required) - true if the flight has been cancelled
        + source: `flightiew` (RealtimeStatusSource, required) - indicates source of the real time data. 
        + last_updated: `2017-05-08T19:30:00Z` (timestamp, required) - time at which this real time status was most recently updated
    + weather (object, optional) - weather information for the origin and destination airports.
        + origin (WeatherForecast, required) - weather forecast at origin airport at time of departure, or on departure day (as available)
        + destination (WeatherForecast, required) - weather forecast at destination airport at time of arrival, or on arrival day (whichever is available)

## PushNotificationChange (enum[string])
A reason for the push notification getting sent, representing a change in the real time status for a flight (such as a delay issue by the airline), or a change in Flightsayer's prediction, etc.
### Members
+ `eta-etd-update` - there has been a change in the real-time estimated arrival or departure time.
+ `terminal-gate-change` - the terminal, gate, or baggage claim has changed
+ `incoming-eta-etd-update` - there has been a change in the real-time estimated arrival or departure time for the incoming flight to this flight.
+ `incoming-aircraft-change` - the incoming flight for this flight has changed to a different aircraft. This indicates a schedule disruption and a potential delay.
+ `delay-index-update` - the delay index for this flight has changed
+ `initial-status` - this is the initial status of the flight, sent after the subscribe request

## ConnectionInfo (object)
Shows the liklihood of missing a connection at a particular airport, based on Flightsayer's predictions and the latest flight status. This data is only present if an itinerary contains at least one connection.

+ Attributes
    + airport: `ORD` (string, required) - airport code for an airport that is a connection in this itinerary
    + prob_miss: `0.2` (number, required) - probability of missing a connection
    + connection_time: `40` (number, required) - the number of minutes available to make the connection, as of the latest published real time status. Note that it's possible to have a large probability of a missed connection *and* and long connection time. This occurs when Flightsayer is predicting a delay in the inbound flight, but that delay has not yet been declared in the flight schedule.

## timestamp (string)
A timestamp in ISO-8601 format, for example: `2016-09-09T15:00:00Z` or `2016-08-25T20:35:00-00:05`. Note that the time zone offset is included for timestamps representing an arrival or departure time, and the offset is the local time at the corresponding origin or destination airport.

## AirportInfo (object)
Information about an airport

+ Attributes

    + iata: `ORD`(string, required) - IATA airport code
    + city: `Chicago` (string, required) - airport city name
    + name: `Chicago O'Hare Intl` (string, required) - full airport name

## DelayCause (enum[string])
A reason for the delay prediction. Note that a value of null means that the flight departure is too far out to have a specific cause for delay. Within ~24 hours, delay casues kick in. 
### Members
+ `flight-cancelled` - cancellation source is either swim or flightview
+ `late-incoming-flight` - late inbound flight
+ `arrival-airport-conditions` - ground delay program (GDP) of ground stop (GS) at the scheduled arrival airport, or flight has a controlled time of arrival.
+ `arrival-airport-constraints` - arrival volume constraints: no GDP/GS but high volume (typically happens immediately after a GDP ends)
+ `departure-airport-constraints` - departure constraints: volume exceeds capacity
+ `latest-available-information` - based on latest swim or flightview ETA/ETD. This is a catch all of all airline delays like maintenance.

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
+ `In Air` - flight is in the air (in many cases the wheels up time is delayed 5 minutes in accordance with FAA regulations.)
+ `Landed` - flight has landed
+ `Arrived` - flight has arrived at the arrival gate
+ `Expected` - the flight is expected to land at the arrival airport soon
+ `Delayed` - the flight has been delayed by the airline or the FAA.
+ `Cancelled` - the flight has been cancelled
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
