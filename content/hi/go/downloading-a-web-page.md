---
title:                "एक वेब पेज को डाउनलोड करना"
html_title:           "Go: एक वेब पेज को डाउनलोड करना"
simple_title:         "एक वेब पेज को डाउनलोड करना"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Kyun?

Jab aap internet par surf karte hain, aapne shayad notice kiya hoga ki kuch websites ke content ko offline mode mein bhi access kiya ja sakta hai. Iska sabse common example hai web pages ko download karna. Aap kuch important information ko offline mode mein access kar sakte hain jab aap koi web page download karte hain. Iss tarah se aap bina internet connection ke bhi web page par content ko padh sakte hain.

## Kaise Karein?

Web page ko download karne ke liye, aapko Go programming language ka use karna hoga. Yahaan mein aapko kuch simple steps batane jaa raha hu jisse aap web page download kar sakte hain.

Sabse pehle, aapko "net/http" library ko import karna hoga. Iss library mein "Get" function ko use karke aap kisi bhi website ka content download kar sakte hain. Jaise ki, ```Go
res, err := http.Get("website ka URL")
```
Iske baad, aap "io/ioutil" library ko import karna hoga, jisse aap downloaded content ko read karna sakte hain. Jaise ki, ```Go
data, err := ioutil.ReadAll(res.Body)
```
Iss tarah aap downloaded content ko access kar sakte hain. Code ka sample output neeche diya gaya hai.

```Go
package main

import (
	"fmt"
	"io/ioutil"
	"net/http"
)

func main() {
	res, err := http.Get("https://www.google.com")
	if err != nil {
		fmt.Println("Error while downloading!")
	}
	data, err := ioutil.ReadAll(res.Body)
	if err != nil {
		fmt.Println("Error while reading!")
	}
	fmt.Println(string(data))
}
```

## Deep Dive

HTTP library mein "Get" function ka use karne se pehle, humein website ka URL specify karna hota hai. Isse hum request ko server tak pahunchate hain aur usse website ka content download karte hain. Iss process mein, humein ek response object aur ek error object milta hai. Agar response me koi bhi error hota hai, toh hum us error ko handle kar sakte hain. Agar sab kuch sahi hota hai, toh "ioutil" library se hum downloaded content ko read kar sakte hain.

## Zaroor Dekhein

* Go Official Website: https://golang.org/
* Net/Http Library Documentation: https://golang.org/pkg/net/http/
* Ioutil Library Documentation: https://golang.org/pkg/io/ioutil/

## Ona