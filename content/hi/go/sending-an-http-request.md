---
title:                "एक एचटीटीपी अनुरोध भेजना"
html_title:           "Go: एक एचटीटीपी अनुरोध भेजना"
simple_title:         "एक एचटीटीपी अनुरोध भेजना"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Kyon

**HTTP** (Hypertext Transfer Protocol) request bhejna kisi bhi network communication mein bahut important hota hai. Yeh ek sadharan si si vasee ***foundation pillar*** hai web development ka, isiliye jaankari honi chahiye ki ye request kaise bheji jaati hai.

## Kaise

```Go
req, err := http.NewRequest("GET", "https://www.example.com", nil)
if err != nil {
    log.Fatal("NewRequest: ", err)
}
resp, err := http.DefaultClient.Do(req)
if err != nil {
    log.Fatal("Do: ", err)
}
defer resp.Body.Close()

// Response output
fmt.Println(resp.Status)
fmt.Println(resp.StatusCode)
fmt.Println(resp.Proto)
```

**Output:**

```Go
200 OK
200
HTTP/2.0
```

**Note:** Iss example mein humne ek naya HTTP request bhejne ke liye `http.NewRequest()` method ka istemaal kiya hai. Fir ek default `http.Client` ke through humne request ko bheja hai `Do()` method ki madad se. Yeh ek basic example hai, lekin aap request ko customize kar sakte hai by adding headers, cookies, etc. Yaha par hamne ek `GET` request bheja hai, lekin aap `POST`, `PUT`, `DELETE`, etc. bhi bhej sakte hai.

## Deep Dive

**HTTP request** ka basic structure ho jata hai 3 parts mein: request line, headers, and body. Request line mein method, URL aur protocol version hoti hai. Headers mein hum kuch specific information, jaise ki `Content-Type` ya `User-Agent` ke baare mein bataate hai. Aur body mein hum data ko bhejte hai, jaise ki form data ya JSON.

HTTP request bhejne ke liye bahut saare options hai Go mein, jaise ki `http.Get()`, `http.Post()`, `http.Do()` ya `http.Client`. Aap in options ki madad se apni needs ke according request bhej sakte hai.

## See Also

- [Official Go Documentation on HTTP](https://golang.org/pkg/net/http/)
- [A Beginner's Guide to HTTP and REST](https://www.smashingmagazine.com/2018/01/understanding-using-rest-api/)
- [How to Build Basic HTTP Servers with Go](https://www.digitalocean.com/community/tutorials/how-to-build-basic-http-server-with-golang)