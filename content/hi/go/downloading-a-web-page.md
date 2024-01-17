---
title:                "वेब पेज डाउनलोड करना"
html_title:           "Go: वेब पेज डाउनलोड करना"
simple_title:         "वेब पेज डाउनलोड करना"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Kya Aur Kyun?
Web page download karna programming ka ek aam kaam hai jisme ham web se koi bhi page ko apne computer par save kar sakte hain. Programmers ise tab karte hain jab unhe kisi website ya web application ke data ya content ka access chahiye hota hai jisko we offline use mein kar sakte hain.

# Kaise?
```Go
import (
  "fmt"
  "net/http"
  "io/ioutil"
)

func main() {
  // URL jiska data download karna hai
  url := "https://example.com"

  // Web page ko download karne ke liye request bhejna
  response, err := http.Get(url)
  
  if err != nil {
    fmt.Println("Error reading web page:", err)
    return
  }
  
  // Response ka body read karke print karna
  body, err := ioutil.ReadAll(response.Body)
  
  if err != nil {
    fmt.Println("Error reading response:", err)
    return
  }
  
  fmt.Println(string(body))
}
```
OUTPUT:
```
<HTML>
  <HEAD>
    <title>Example Domain</title>
    ...
  </HEAD>
  
  <BODY>
    <H1>Example Domain</H1>
    <P>This domain is for use in illustrative examples in documents. You may use this domain in literature without prior coordination or asking for permission.</P>
    ...
  </BODY>
</HTML>
```

# Gehri Jhaanki
Web page download ka concept hua hai jab internet ke shuruaat mein log apne computer par files ko download karne ke liye FTP (File Transfer Protocol) ka istemal karte the. Lekin aaj ke zamaneme, browsers aur web technologies ke development se, web page download kaafi aasaan ho gaya hai aur languages jaise Go, ismein capabilities ko improve karne mein madad karte hain. Jaise dusre languages, Go mein bhi alag-alag libraries aur tools available hain jinse web page ko download kar sakte hain.

# Dekhein Bhi
- [Go documentation](https://golang.org/pkg/net/http/)
- [Downloading files with Go](https://golangbot.com/download-file-from-website/)
- [How to use Go to make HTTP requests](https://www.digitalocean.com/community/tutorials/how-to-use-go-http-clients-to-create-web-applications)