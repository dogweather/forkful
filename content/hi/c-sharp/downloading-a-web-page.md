---
title:                "वेब पेज डाउनलोड करना"
html_title:           "C#: वेब पेज डाउनलोड करना"
simple_title:         "वेब पेज डाउनलोड करना"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Kyon

Download karna, yani web page ko apne computer ya device mein save karna, kaafi upyogi ho sakta hai. Aap jab bhi internet se offline ho jaate hain, tab aap downloaded web pages ko access kar sakte hain aur online content ke bina bhi information ko padh sakte hain.

## Kaise

```C#
// C# mein ek web page download karne ka simple code
using System; // namespace for C# libraries

using System.Net; // namespace for web requests

public class Program
{
    public static void Main()
    {
        string url = "https://www.google.com/"; // specify the URL of the web page you want to download
        WebClient client = new WebClient(); // create a new web client object
        string html = client.DownloadString(url); // use the DownloadString method to download the web page as a string
        Console.WriteLine(html); // print the downloaded web page
    }
}

```

**Output:**

```
<!doctype html>
<html>
<head>
<title>Google</title>
...
...
...</html>
```

Is code mein humne pehle ek variable mein web page ka URL store kiya, phir ek web client object create kiya aur uska DownloadString method use kiya. Iss tarah se humne web page ko string format mein download kiya aur console par print kiya.

## Deep Dive

Web pages ko download karna ke liye bahut saari C# libraries aur methods available hain. Hum WebClient class ka use kiya hai, par hum HttpWebRequest aur HttpClient classes ka bhi use kar sakte hain. In classes mein bahut saari methods available hain jo web pages ko download karne aur manipulate karne mein help karte hain.

Ek aur important point yeh hai ki web pages do tarah ke hote hain - static aur dynamic. Static web pages hamesha same hote hain jab aap unhe access karte hain. Par dynamic web pages hamesha change hote rehte hain. Humari code mein humne ek static web page download kiya hai, par dynamic web pages ko download karne mein hume kuch aur methods ka use karna pad sakta hai.

## Dekho Bhi

- [C# Programming Language](https://en.wikipedia.org/wiki/C_Sharp_(programming_language))
- [WebClient Class](https://docs.microsoft.com/en-us/dotnet/api/system.net.webclient?view=netcore-3.1)
- [HttpWebRequest Class](https://docs.microsoft.com/en-us/dotnet/api/system.net.httpwebrequest?view=netcore-3.1)
- [HttpClient Class](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient?view=netcore-3.1)