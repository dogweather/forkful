---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:51:42.916205-06:00
description: "\u0E01\u0E32\u0E23\u0E40\u0E23\u0E34\u0E48\u0E21\u0E15\u0E49\u0E19\u0E42\
  \u0E1B\u0E23\u0E40\u0E08\u0E04\u0E43\u0E2B\u0E21\u0E48\u0E2B\u0E21\u0E32\u0E22\u0E16\
  \u0E36\u0E07\u0E01\u0E32\u0E23\u0E15\u0E31\u0E49\u0E07\u0E04\u0E48\u0E32\u0E2A\u0E20\
  \u0E32\u0E1E\u0E41\u0E27\u0E14\u0E25\u0E49\u0E2D\u0E21\u0E01\u0E32\u0E23\u0E40\u0E02\
  \u0E35\u0E22\u0E19\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E43\u0E2B\u0E21\u0E48\
  \u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E42\u0E04\u0E49\u0E14\u0E02\u0E2D\u0E07\u0E04\
  \u0E38\u0E13\u2026"
lastmod: '2024-03-17T21:57:55.808262-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E40\u0E23\u0E34\u0E48\u0E21\u0E15\u0E49\u0E19\u0E42\
  \u0E1B\u0E23\u0E40\u0E08\u0E04\u0E43\u0E2B\u0E21\u0E48\u0E2B\u0E21\u0E32\u0E22\u0E16\
  \u0E36\u0E07\u0E01\u0E32\u0E23\u0E15\u0E31\u0E49\u0E07\u0E04\u0E48\u0E32\u0E2A\u0E20\
  \u0E32\u0E1E\u0E41\u0E27\u0E14\u0E25\u0E49\u0E2D\u0E21\u0E01\u0E32\u0E23\u0E40\u0E02\
  \u0E35\u0E22\u0E19\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E43\u0E2B\u0E21\u0E48\
  \u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E42\u0E04\u0E49\u0E14\u0E02\u0E2D\u0E07\u0E04\
  \u0E38\u0E13 \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\
  \u0E17\u0E33\u0E40\u0E0A\u0E48\u0E19\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\
  \u0E40\u0E23\u0E34\u0E48\u0E21\u0E15\u0E49\u0E19\u0E01\u0E32\u0E23\u0E1E\u0E31\u0E12\
  \u0E19\u0E32\u0E14\u0E49\u0E27\u0E22\u0E20\u0E39\u0E21\u0E34\u0E1B\u0E23\u0E30\u0E40\
  \u0E17\u0E28\u0E17\u0E35\u0E48\u0E2A\u0E30\u0E2D\u0E32\u0E14\u0E41\u0E25\u0E30\u0E08\
  \u0E31\u0E14\u0E23\u0E30\u0E40\u0E1A\u0E35\u0E22\u0E1A\u0E04\u0E27\u0E32\u0E21\u0E04\
  \u0E34\u0E14\u0E40\u0E02\u0E49\u0E32\u0E2A\u0E39\u0E48\u0E42\u0E04\u0E49\u0E14\u0E17\
  \u0E35\u0E48\u0E08\u0E31\u0E1A\u0E15\u0E49\u0E2D\u0E07\u0E44\u0E14\u0E49."
title: "\u0E40\u0E23\u0E34\u0E48\u0E21\u0E15\u0E49\u0E19\u0E42\u0E04\u0E23\u0E07\u0E01\
  \u0E32\u0E23\u0E43\u0E2B\u0E21\u0E48"
weight: 1
---

## อะไร & ทำไม?

การเริ่มต้นโปรเจคใหม่หมายถึงการตั้งค่าสภาพแวดล้อมการเขียนโปรแกรมใหม่สำหรับโค้ดของคุณ โปรแกรมเมอร์ทำเช่นนี้เพื่อเริ่มต้นการพัฒนาด้วยภูมิประเทศที่สะอาดและจัดระเบียบความคิดเข้าสู่โค้ดที่จับต้องได้

## วิธีการ:

เพื่อ bootstrap โปรเจค Clojure, เราจะใช้ Leiningen, เครื่องมือสร้างที่ได้รับความนิยมสำหรับ Clojure:

``` Clojure
;; 1. ติดตั้ง Leiningen หากคุณยังไม่ได้ติดตั้ง (https://leiningen.org/)
;; 2. สร้างโครงกระดูกโปรเจคใหม่:
lein new app my-cool-app

;; 3. นำทางไปยังโปรเจคใหม่ของคุณ:
cd my-cool-app

;; 4. เริ่ม REPL (Read-Eval-Print Loop):
lein repl

;; ตัวอย่างผลลัพธ์:
;; nREPL server started on port 12345 on host 127.0.0.1 - nrepl://127.0.0.1:12345
;; REPL-y 0.4.4, nREPL 0.6.0
;; Clojure 1.10.1
;; Java 1.8.0_232
;;     Docs: (doc function-name-here)
;;           (find-doc "part-of-name-here")
;;   Source: (source function-name-here)
;;  Javadoc: (javadoc java-object-or-class-here)
;;     Exit: Control+D or (exit) or (quit)
;;  ผลลัพธ์: จัดเก็บใน vars *1, *2, *3, ถ้าเกิดข้อผิดพลาดใน *e

;; 5. สร้างไฟล์สำหรับโค้ดของคุณ (src/my_cool_app/core.clj) และเปิดในโปรแกรมแก้ไขข้อความที่คุณชอบ

;; 6. เขียนโค้ด Clojure ง่ายๆ:
(ns my-cool-app.core)

(defn say-hello []
  (println "Hello, Clojure world!"))

;; 7. รันฟังก์ชันของคุณใน REPL:
(my-cool-app.core/say-hello)

;; ตัวอย่างผลลัพธ์:
;; Hello, Clojure world!
```

## ลงลึก

โปรเจค Clojure มักเริ่มต้นด้วย Leiningen หรือ Boot สำหรับการจัดการ dependencies, การสร้าง, และการทำงานแบบอัตโนมัติ Leiningen ได้เริ่มต้นมาตั้งแต่ 2010 และได้เป็นตัวเลือกมาตรฐานสำหรับ Clojurists ส่วนใหญ่

มีเครื่องมืออื่นๆ ที่มีอยู่ เช่น `deps.edn` และเครื่องมือ Clojure CLI ซึ่งถูกนำเสนอโดย Clojure/core เพื่อให้การจัดการ dependency และการกำหนดค่าโปรเจคเป็นไปอย่างง่ายดาย

Clojure เองให้คุณค่ากับความไม่เปลี่ยนแปลงและการเขียนโปรแกรมแบบฟังก์ชัน การเริ่มต้นโปรเจคอย่างถูกต้องเน้นการจัดการสถานะที่สะอาดและการแยกปัญหาออกจากฟังก์ชันและเนมสเปซต่างๆ

โปรเจคมักจะยึดตามโครงสร้างไดเรกทอรีมาตรฐาน:
- `src/` สำหรับโค้ดหลักของคุณ
- `test/` สำหรับโค้ดทดสอบ
- `resources/` สำหรับทรัพยากรที่ไม่ใช่โค้ด
- `project.clj` หรือ `deps.edn` เพื่อจัดการ dependencies และการกำหนดค่า

ปฏิบัติที่ดีคือการเก็บสิ่งต่างๆ เรียบง่ายในตอนเริ่มต้น เพิ่ม dependencies ตามที่คุณไป ทำให้โปรเจคของคุณเบาและสามารถจัดการได้

## ดูเพิ่มเติม

- [แนะนำการเริ่มต้นใช้งาน Leiningen](https://leiningen.org/#getting-started)
- [เอกสาร Clojure](https://clojuredocs.org/)
- [คู่มือสไตล์ Clojure](https://guide.clojure.style/)
- [เครื่องมือ Clojure CLI](https://clojure.org/guides/getting_started)
- [Clojure Toolbox](https://www.clojure-toolbox.com/)
