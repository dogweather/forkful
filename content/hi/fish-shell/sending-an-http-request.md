---
title:                "एक http अनुरोध भेजना"
html_title:           "Fish Shell: एक http अनुरोध भेजना"
simple_title:         "एक http अनुरोध भेजना"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/sending-an-http-request.md"
---

{{< edit_this_page >}}

**What & Why?**
Sukh Mantra ka - HTTP niwedan ko bhejana aur isakee avadharana kaise kaam karatee hai.

HTTP niwedan bhejane ka arth hai ek server se web URL ki madad se jaankari maangana. Yeh Hindi mein 'चालू' karana hai. Iska sabse bada fayda yeh hai ki aap apne browser ke madhyaman se nishchit URL par ja sakte hain, aur isase aap ko jyada ek web server par kaam karane ki aavashyakata nahin hai.

**How to:**
Fish Shell mein HTTP niwedan kaise bhejen, yeh aap apane koonar mein aasanee se kar sakate hain. Aap ko sirph yeh karna hai:

```Fish Shell
curl <URL>
```
Yeh command, server se kaheegi chetavane nikalane ke liye jaancha hua hai aur aap server se dastaaniyan bana sakate hain.

```Fish Shell
curl -i <URL>
```
Isase aap ko server se jaanana lagata hai ki aap kaun hain aur aap is niwedan ko kyun bhejen.

**Deep Dive:**
HTTP niwedan ki abaadi Mein bahut Jain paramparae hai, isake liye aur se hu.

- Aap Res ka API niwedan API ka jad hai aur yeh programmatics roop se jaanana kisi bhi prakar se saral banaana padega. Yahan par API ka jad Adh Jal Prayog (अध्यापकीय प्रयोग) mein aage chalate Hain tatha is prakar ek ched.

**See Also:**
HTTP niwedan bhejane se sambandhit aur bhi kuchh aadhaarbo dvara Is article mein (कुछ और उदाहरण) jo use kie gae:
- [POSTman:](https://www.getpostman.com/) ek vyapak onaline tool jo HTTP niwedano ko sashikat banaata hai aur test karta hai
- [cURL:](https://curl.haxx.se/) ek sulai se Jodee gaye command line HTTP clients, jinamen se ek Fish Shell mein aavashyak dastaanee bhi Diya gaya hai.
- [HTTP clients:](https://tools.ietf.org/html/rfc7230) sudhi siddhantaan HTTP niwedan bhejane ke Http siddhantaano mein ek suad bank ke silsile mein adhunik vatavaran dastaan milap Yahan ke bare mein aapako HTML 1 sa Roth ke sang sandar bana sakte hain.