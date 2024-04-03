---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:48:36.865104-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: Clojure \u0E44\u0E21\u0E48\
  \u0E21\u0E35\u0E04\u0E27\u0E32\u0E21\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E43\u0E19\
  \u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\
  \u0E4C HTML \u0E17\u0E35\u0E48\u0E15\u0E34\u0E14\u0E15\u0E31\u0E49\u0E07\u0E21\u0E32\
  \u0E1E\u0E23\u0E49\u0E2D\u0E21\u0E01\u0E31\u0E1A\u0E23\u0E30\u0E1A\u0E1A, \u0E41\
  \u0E15\u0E48\u0E04\u0E38\u0E13\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E43\u0E0A\u0E49\
  \u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\u0E35\u0E02\u0E2D\u0E07 Java \u0E2B\u0E23\u0E37\
  \u0E2D Clojure wrapper \u0E40\u0E0A\u0E48\u0E19 `enlive` \u0E2B\u0E23\u0E37\u0E2D\
  \u2026"
lastmod: '2024-03-17T21:57:55.804590-06:00'
model: gpt-4-0125-preview
summary: "Clojure \u0E44\u0E21\u0E48\u0E21\u0E35\u0E04\u0E27\u0E32\u0E21\u0E2A\u0E32\
  \u0E21\u0E32\u0E23\u0E16\u0E43\u0E19\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E27\u0E34\
  \u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\u0E4C HTML \u0E17\u0E35\u0E48\u0E15\u0E34\u0E14\
  \u0E15\u0E31\u0E49\u0E07\u0E21\u0E32\u0E1E\u0E23\u0E49\u0E2D\u0E21\u0E01\u0E31\u0E1A\
  \u0E23\u0E30\u0E1A\u0E1A, \u0E41\u0E15\u0E48\u0E04\u0E38\u0E13\u0E2A\u0E32\u0E21\
  \u0E32\u0E23\u0E16\u0E43\u0E0A\u0E49\u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\u0E35\u0E02\
  \u0E2D\u0E07 Java \u0E2B\u0E23\u0E37\u0E2D Clojure wrapper \u0E40\u0E0A\u0E48\u0E19\
  \ `enlive` \u0E2B\u0E23\u0E37\u0E2D `hickory`."
title: "\u0E01\u0E32\u0E23\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\u0E4C HTML"
weight: 43
---

## วิธีการ:
Clojure ไม่มีความสามารถในการแยกวิเคราะห์ HTML ที่ติดตั้งมาพร้อมกับระบบ, แต่คุณสามารถใช้ไลบรารีของ Java หรือ Clojure wrapper เช่น `enlive` หรือ `hickory`. นี่คือวิธีการใช้ทั้งสอง:

### การใช้ Enlive:
Enlive เป็นตัวเลือกที่นิยมสำหรับการแยกวิเคราะห์ HTML และการเก็บข้อมูลจากเว็บ. ขั้นแรก, รวมมันเข้ากับ dependencies ของโครงการของคุณ:

```clojure
[net.cgrand/enlive "1.1.6"]
```

จากนั้น, คุณสามารถแยกวิเคราะห์และนำทาง HTML ได้ดังนี้:

```clojure
(require '[net.cgrand.enlive-html :as html])

(let [doc (html/html-resource (java.net.URL. "http://example.com"))]
  (html/select doc [:div.some-class]))
```

ตัวอย่างนี้ดึงหน้า HTML และเลือกทุก `<div>` ที่มีคลาส `some-class`.

ผลลัพธ์อาจมีลักษณะดังนี้:

```clojure
({:tag :div, :attrs {:class "some-class"}, :content ["Here's some content."]})
```

### การใช้ Hickory:
Hickory ให้วิธีการแยกวิเคราะห์ HTML เป็นรูปแบบที่ง่ายต่อการใช้งานใน Clojure มากขึ้น. เพิ่ม Hickory เข้ากับ dependencies ของโครงการคุณ:

```clojure
[hickory "0.7.1"]
```

นี่คือตัวอย่างง่ายๆ:

```clojure
(require '[hickory.core :as hickory]
         '[hickory.select :as select])

;; แยกวิเคราะห์ HTML เป็นรูปแบบ Hickory
(let [doc (hickory/parse "<html><body><div id='main'>Hello, world!</div></body></html>")]
  ;; เลือก div ที่มี id 'main'
  (select/select (select/id "main") doc))
```

โค้ดนี้แยกวิเคราะห์สตริง HTML ง่ายๆ และใช้ CSS selector หา `div` ที่มี ID `main`.

ผลลัพธ์ตัวอย่าง:

```clojure
[{:type :element, :tag :div, :attrs {:id "main"}, :content ["Hello, world!"]}]
```

ทั้ง `enlive` และ `hickory` เสนอวิธีการที่มั่นคงสำหรับการแยกวิเคราะห์ HTML ใน Clojure, โดย `enlive` มุ่งเน้นไปที่การสร้างเทมเพลตและ `hickory` มุ่งเน้นไปที่การเปลี่ยนแปลงข้อมูล.
