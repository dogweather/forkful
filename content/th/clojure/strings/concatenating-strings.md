---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:45:41.272425-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E17\u0E33: Clojure \u0E17\u0E33\u0E43\u0E2B\
  \u0E49\u0E01\u0E32\u0E23\u0E15\u0E48\u0E2D\u0E40\u0E0A\u0E37\u0E2D\u0E01\u0E02\u0E49\
  \u0E2D\u0E04\u0E27\u0E32\u0E21\u0E40\u0E1B\u0E47\u0E19\u0E40\u0E23\u0E37\u0E48\u0E2D\
  \u0E07\u0E07\u0E48\u0E32\u0E22\u0E14\u0E49\u0E27\u0E22\u0E1F\u0E31\u0E07\u0E01\u0E4C\
  \u0E0A\u0E31\u0E19 `str` \u0E25\u0E2D\u0E07\u0E14\u0E39\u0E01\u0E31\u0E19\u0E40\u0E25\
  \u0E22."
lastmod: '2024-03-17T21:57:55.797763-06:00'
model: gpt-4-0125-preview
summary: "Clojure \u0E17\u0E33\u0E43\u0E2B\u0E49\u0E01\u0E32\u0E23\u0E15\u0E48\u0E2D\
  \u0E40\u0E0A\u0E37\u0E2D\u0E01\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E40\u0E1B\
  \u0E47\u0E19\u0E40\u0E23\u0E37\u0E48\u0E2D\u0E07\u0E07\u0E48\u0E32\u0E22\u0E14\u0E49\
  \u0E27\u0E22\u0E1F\u0E31\u0E07\u0E01\u0E4C\u0E0A\u0E31\u0E19 `str` \u0E25\u0E2D\u0E07\
  \u0E14\u0E39\u0E01\u0E31\u0E19\u0E40\u0E25\u0E22."
title: "\u0E01\u0E32\u0E23\u0E15\u0E48\u0E2D\u0E2A\u0E15\u0E23\u0E34\u0E07"
weight: 3
---

## วิธีทำ:
Clojure ทำให้การต่อเชือกข้อความเป็นเรื่องง่ายด้วยฟังก์ชัน `str` ลองดูกันเลย:

```clojure
;; การต่อข้อความง่าย ๆ ด้วยฟังก์ชัน str
(str "Hello, " "world!")
;; => "Hello, world!"

;; การต่อหลาย ๆ ข้อความ
(str "Clojure" " is" " awesome!")
;; => "Clojure is awesome!"

;; การรวมข้อความและค่าอื่น ๆ
(str "The answer is " 42)
;; => "The answer is 42"

;; การใช้ apply เพื่อต่อลำดับของข้อความ
(apply str ["Join" " " "these" " " "strings!"])
;; => "Join these strings!"
```

ดีเลย, คุณได้เห็นมันในการทำงานแล้ว จำไว้เพียงว่า `str` ทำงานกับค่าใด ๆ โดยการเรียก `toString` บนค่านั้น ถ้าเป็น nil, คุณจะได้รับข้อความ "nil"

## ลงลึก
ในประวัติศาสตร์, การต่อเชือกของข้อความได้มีมาตั้งแต่เราต้องการจัดการข้อความโดยเชิงโปรแกรม และแต่ละภาษานำเสนอวิธีของตนเอง Clojure ใน `str` เป็นส่วนหนึ่งของไลบรารีหลัก ถูกนำเข้ามาเพื่อความเรียบง่ายและเป็นเอกภาพ

มีทางเลือกอื่น ๆ สำหรับ `str` หรือ? ใช่! `StringBuilder` อาจมีประสิทธิภาพสูงขึ้นสำหรับการต่อเชือกข้อความจำนวนมาก โดยเฉพาะอย่างยิ่งในลูป Clojure สามารถเรียกเมธอดของ Java ได้ ดังนั้นคุณสามารถใช้ `StringBuilder` ได้ด้วย:

```clojure
;; การใช้ StringBuilder เพื่อความมีประสิทธิภาพ
(let [builder (StringBuilder.)]
  (.append builder "This is")
  (.append builder " a more")
  (.append builder " efficient way!")
  (.toString builder))
;; => "This is a more efficient way!"
```

ทำไมไม่ใช้ `StringBuilder` ตลอดเวลาล่ะ? สำหรับงานประจำวันส่วนใหญ่, `str` นั้นเรียบง่ายและเร็วพอ ๆ `StringBuilder` โดดเด่นในสถานการณ์ที่มีประสิทธิภาพสูงกับการต่อเชือกหลายครั้ง

ในแง่ของการเนียน, เนื่องจาก Clojure ใช้งานบน JVM, จึงได้รับประโยชน์จากความสามารถในการจัดการสตริงของ Java อย่างไรก็ตาม, เช่นเดียวกับใน Java `String`s, การเรียก `str` แต่ละครั้งสร้าง `String` ใหม่, ซึ่งอาจเป็นปัจจัยที่ต้องพิจารณาเรื่องหน่วยความจำ

## ดูเพิ่มเติม
- เอกสารของฟังก์ชัน `str` ใน Clojure: [Clojure Strings](https://clojuredocs.org/clojure.core/str)
- เอกสารของ `StringBuilder` ใน Java: [StringBuilder Docs](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/StringBuilder.html)
- คู่มือ Clojure ที่เป็นประโยชน์สำหรับ `str` และอื่น ๆ: [Clojure for the Brave and True](https://www.braveclojure.com/clojure-for-the-brave-and-true/)
