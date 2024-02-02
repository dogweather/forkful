---
title:                "Sending an HTTP request"
date:                  2024-02-01T13:31:52.506981-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sending an HTTP request"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/vba/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why?

Sending an HTTP request with Visual Basic for Applications (VBA) is how we ask the internet to fetch us data or perform an action elsewhere. It's the backbone of pulling live information into your Excel sheets or automating interactions with web services without leaving your comfy Office suite.

## How to:

VBA doesn't naturally embrace internet protocols, but with a little help from Microsoft's XMLHTTP object, it's like teaching an old dog a neat new trick. Here's a simple GET request to grab some data:

```Visual Basic for Application
Sub SendGetRequest()
    Dim httpRequest As Object
    Set httpRequest = CreateObject("MSXML2.XMLHTTP")
    
    ' Target URL
    httpRequest.Open "GET", "http://example.com/api/data", False
    httpRequest.Send
    
    ' Output the response
    Debug.Print httpRequest.responseText
End Sub
```

To run this, pop open your VBA editor in Excel, paste this subroutine, and press F5. If all goes well, the Immediate window will light up with the requested data.

For a POST request, things get slightly more intricate due to the need for sending data:

```basic
Sub SendPostRequest()
    Dim httpRequest As Object
    Set httpRequest = CreateObject("MSXML2.XMLHTTP")
    Dim postData As String
    
    ' Your data to send
    postData = "key1=value1&key2=value2"
    
    ' Setup and send
    httpRequest.Open "POST", "http://example.com/api/submit", False
    httpRequest.setRequestHeader "Content-Type", "application/x-www-form-urlencoded"
    httpRequest.Send (postData)
    
    ' Check out the response
    Debug.Print httpRequest.Status & ": " & httpRequest.responseText
End Sub
```

This time, replace the URL with a legit endpoint that accepts POST requests, adjust the `postData` string with your actual data, and let it rip.

## Deep Dive

Before the days of integrated development environments that could speak HTTP as if it were their mother tongue, VBA was mostly confined to desktop applications without a straightforward path to the internet. The introduction of the XMLHTTP object as part of the MSXML library was a game-changer, allowing VBA to perform actions over the web.

However, it's worth mentioning that while VBA can send HTTP requests, it's not the most modern or powerful way to interact with web services. Languages designed with the internet in mind, such as Python with its Requests library or JavaScript running on Node.js, offer a far more flexible and intuitive approach to dealing with HTTP requests and handling their responses.

VBA's approach, using the XMLHTTP object, is a bit like retrofitting a vintage car with a modern engine; it works, and it can be pretty cool for specific tasks, especially if you're working within an Office-centric environment. But for heavy lifting on the web, exploring newer, more web-native options might be the way to go.
