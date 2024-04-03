---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:50:46.105249-06:00
description: "\u0E01\u0E32\u0E23\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP \u0E40\
  \u0E1B\u0E47\u0E19\u0E27\u0E34\u0E18\u0E35\u0E17\u0E35\u0E48\u0E42\u0E1B\u0E23\u0E41\
  \u0E01\u0E23\u0E21\u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13\u0E02\u0E2D\u0E02\u0E49\u0E2D\
  \u0E21\u0E39\u0E25\u0E2B\u0E23\u0E37\u0E2D\u0E1A\u0E23\u0E34\u0E01\u0E32\u0E23\u0E08\
  \u0E32\u0E01\u0E23\u0E30\u0E1A\u0E1A\u0E2D\u0E37\u0E48\u0E19\u0E1C\u0E48\u0E32\u0E19\
  \u0E40\u0E27\u0E47\u0E1A \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\
  \u0E23\u0E4C\u0E17\u0E33\u0E40\u0E0A\u0E48\u0E19\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\
  \u0E48\u0E2D\u0E42\u0E15\u0E49\u0E15\u0E2D\u0E1A\u0E01\u0E31\u0E1A web APIs, \u0E14\
  \u0E36\u0E07\u0E17\u0E23\u0E31\u0E1E\u0E22\u0E32\u0E01\u0E23,\u2026"
lastmod: '2024-03-17T21:57:55.803578-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP \u0E40\
  \u0E1B\u0E47\u0E19\u0E27\u0E34\u0E18\u0E35\u0E17\u0E35\u0E48\u0E42\u0E1B\u0E23\u0E41\
  \u0E01\u0E23\u0E21\u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13\u0E02\u0E2D\u0E02\u0E49\u0E2D\
  \u0E21\u0E39\u0E25\u0E2B\u0E23\u0E37\u0E2D\u0E1A\u0E23\u0E34\u0E01\u0E32\u0E23\u0E08\
  \u0E32\u0E01\u0E23\u0E30\u0E1A\u0E1A\u0E2D\u0E37\u0E48\u0E19\u0E1C\u0E48\u0E32\u0E19\
  \u0E40\u0E27\u0E47\u0E1A \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\
  \u0E23\u0E4C\u0E17\u0E33\u0E40\u0E0A\u0E48\u0E19\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\
  \u0E48\u0E2D\u0E42\u0E15\u0E49\u0E15\u0E2D\u0E1A\u0E01\u0E31\u0E1A web APIs, \u0E14\
  \u0E36\u0E07\u0E17\u0E23\u0E31\u0E1E\u0E22\u0E32\u0E01\u0E23, \u0E2B\u0E23\u0E37\
  \u0E2D\u0E01\u0E32\u0E23\u0E2A\u0E37\u0E48\u0E2D\u0E2A\u0E32\u0E23\u0E23\u0E30\u0E2B\
  \u0E27\u0E48\u0E32\u0E07\u0E1A\u0E23\u0E34\u0E01\u0E32\u0E23\u0E15\u0E48\u0E32\u0E07\
  \u0E46."
title: "\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP"
weight: 44
---

## อะไรและทำไม?
การส่งคำขอ HTTP เป็นวิธีที่โปรแกรมของคุณขอข้อมูลหรือบริการจากระบบอื่นผ่านเว็บ โปรแกรมเมอร์ทำเช่นนี้เพื่อโต้ตอบกับ web APIs, ดึงทรัพยากร, หรือการสื่อสารระหว่างบริการต่างๆ

## วิธีทำ:
ใน Clojure, คุณสามารถส่งคำขอ HTTP ได้โดยใช้ `clj-http` client.

ก่อนอื่น, เพิ่ม dependency ไปยัง `project.clj` ของคุณ:
```clojure
[clj-http "3.12.3"]
```

ตอนนี้, ไปส่งคำขอ GET กัน:
```clojure
(require '[clj-http.client :as client])

(let [response (client/get "http://httpbin.org/get")]
  (println response))
```

ตัวอย่างผลลัพธ์:
```clojure
{:status 200, :headers {...}, :body "..."}
```

เพื่อโพสต์ข้อมูล:
```clojure
(let [response (client/post "http://httpbin.org/post" {:form-params {:key "value"}})]
  (println response))
```

## ศึกษาเพิ่มเติม
การส่งคำขอ HTTP ไม่ใช่เรื่องใหม่ มันเก่าแก่เท่ากับเว็บเอง Clojure, ซึ่งเป็น Lisp สมัยใหม่, มีหลาย libs ที่ทำให้สามารถส่งคำขอ HTTP ได้ `clj-http` เป็นตัวที่ได้รับความนิยม, แต่ยังมีอื่นๆ เช่น `http-kit` หรือ `clj-http.client` ของ Clojure เอง

`clj-http` ใช้ Apache HttpComponents Client สำหรับ Java ภายใต้ประทุน มันทรงพลังแต่อาจรู้สึกหนักไปทาง Java ทางเลือก, `http-kit`, เป็นไปตามแบบของ Clojure มากกว่าและเบากว่า แต่คุณลักษณะอาจจะน้อยกว่า

เมื่อคุณส่งคำขอ HTTP, คุณกำลังทำเช่นนั้นผ่าน TCP/IP, ซึ่งจะจัดเรียงคำขอของคุณตามโปรโตคอลที่มีมาอย่างยาวนาน มาตรฐานสากลนี้ทำให้คุณสามารถโต้ตอบกับบริการเว็บที่มีอยู่อย่างแท้จริง

## ดูเพิ่มเติม
- พื้นที่เก็บข้อมูล GitHub ของ `clj-http`: https://github.com/dakrone/clj-http
- เว็บไซต์อย่างเป็นทางการของ Clojure: https://clojure.org
- เอกสารของ HttpComponents Client: https://hc.apache.org/httpcomponents-client-ga/
- สำหรับความต้องการแบบเรียลไทม์, พิจารณา `http-kit`: http://www.http-kit.org
