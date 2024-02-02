---
title:                "Sending an HTTP request with basic authentication"
date:                  2024-02-01T13:31:56.471271-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sending an HTTP request with basic authentication"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/vba/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why?

Sending an HTTP request with basic authentication in Visual Basic for Applications (VBA) is about getting your VBA code to talk to a web server securely. Programmers do this to access or modify resources on a server, like reading data or triggering server-side logic, with the peace of mind that their credentials are safely handled.

## How to:

In VBA, to send an HTTP request with basic authentication, you'll typically use the `MSXML2.ServerXMLHTTP` object. You need to encode your username and password into base64 and then attach this as an authorization header. Here's a basic example to get you started:

```basic
Sub SendHTTPRequestWithBasicAuth()
    Dim url As String
    Dim username As String
    Dim password As String
    Dim base64Credentials As String
    Dim xmlhttp As Object

    ' Your web server endpoint
    url = "https://your-api-endpoint.com/data"
    ' Your credentials
    username = "user"
    password = "pass"
    
    ' Create the XMLHTTP object
    Set xmlhttp = CreateObject("MSXML2.ServerXMLHTTP")
    
    ' Base64 encode username and password
    base64Credentials = EncodeBase64(username & ":" & password)
    
    ' Open the request, third argument set to False makes it synchronous
    xmlhttp.Open "GET", url, False
    ' Set the Authorization header with base64 encoded credentials
    xmlhttp.setRequestHeader "Authorization", "Basic " & base64Credentials
    
    ' Send the request
    xmlhttp.send ""
    
    ' Output the response
    Debug.Print xmlhttp.responseText
End Sub

Function EncodeBase64(text As String) As String
    Dim arrData() As Byte
    arrData = StrConv(text, vbFromUnicode)
    EncodeBase64 = Application.WorksheetFunction.EncodeBase64(arrData)
End Function
```

Ensure you replace `https://your-api-endpoint.com/data`, `user`, and `pass` with your endpoint, username, and password.

## Deep Dive

The practice of using basic authentication goes way back in web development. It's still in use due to its straightforward implementation, but it's not considered the most secure method anymore. Basic auth sends credentials in base64 encoding, which can be easily decoded if intercepted. That's why it's paramount to use HTTPS over HTTP when using basic authentication to add a layer of security through encryption.

For VBA, the `MSXML2.ServerXMLHTTP` object is an old but gold way to make HTTP requests. It's widely supported and fairly easy to use for simple needs. However, for more complex scenarios involving advanced security requirements or OAuth, you might find integrating external libraries or using other programming languages more suitable.

Although VBA isn't traditionally seen as a go-to for web interactions, it's perfectly capable of handling tasks like sending authenticated HTTP requests, especially in environments where VBA is already the primary script language, like in Excel or Access applications. Nonetheless, if you're working on a more modern, web-focused project, languages like Python or JavaScript, which have more robust and secure libraries for web requests and authentication, might be a better fit.
