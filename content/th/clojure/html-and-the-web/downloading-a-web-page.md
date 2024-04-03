---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:46:15.002185-06:00
description: "\u0E01\u0E32\u0E23\u0E14\u0E32\u0E27\u0E19\u0E4C\u0E42\u0E2B\u0E25\u0E14\
  \u0E40\u0E27\u0E47\u0E1A\u0E40\u0E1E\u0E08\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\
  \u0E01\u0E32\u0E23\u0E40\u0E01\u0E47\u0E1A HTML \u0E08\u0E32\u0E01 URL \u0E40\u0E1E\
  \u0E37\u0E48\u0E2D\u0E43\u0E2B\u0E49\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E02\
  \u0E2D\u0E07\u0E04\u0E38\u0E13\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E17\u0E33\u0E07\
  \u0E32\u0E19\u0E01\u0E31\u0E1A\u0E21\u0E31\u0E19\u0E44\u0E14\u0E49 \u0E42\u0E1B\u0E23\
  \u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E40\u0E0A\u0E48\
  \u0E19\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E01\u0E32\u0E23\u0E40\u0E01\
  \u0E47\u0E1A\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25, \u0E01\u0E32\u0E23\u0E17\u0E33\
  \u0E07\u0E32\u0E19\u0E2D\u0E31\u0E15\u0E42\u0E19\u0E21\u0E31\u0E15\u0E34\u0E01\u0E31\
  \u0E1A\u0E40\u0E27\u0E47\u0E1A\u2026"
lastmod: '2024-03-17T21:57:55.805607-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E14\u0E32\u0E27\u0E19\u0E4C\u0E42\u0E2B\u0E25\u0E14\
  \u0E40\u0E27\u0E47\u0E1A\u0E40\u0E1E\u0E08\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\
  \u0E01\u0E32\u0E23\u0E40\u0E01\u0E47\u0E1A HTML \u0E08\u0E32\u0E01 URL \u0E40\u0E1E\
  \u0E37\u0E48\u0E2D\u0E43\u0E2B\u0E49\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E02\
  \u0E2D\u0E07\u0E04\u0E38\u0E13\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E17\u0E33\u0E07\
  \u0E32\u0E19\u0E01\u0E31\u0E1A\u0E21\u0E31\u0E19\u0E44\u0E14\u0E49 \u0E42\u0E1B\u0E23\
  \u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E40\u0E0A\u0E48\
  \u0E19\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E01\u0E32\u0E23\u0E40\u0E01\
  \u0E47\u0E1A\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25, \u0E01\u0E32\u0E23\u0E17\u0E33\
  \u0E07\u0E32\u0E19\u0E2D\u0E31\u0E15\u0E42\u0E19\u0E21\u0E31\u0E15\u0E34\u0E01\u0E31\
  \u0E1A\u0E40\u0E27\u0E47\u0E1A \u0E2B\u0E23\u0E37\u0E2D\u0E15\u0E23\u0E27\u0E08\u0E2A\
  \u0E2D\u0E1A\u0E2A\u0E16\u0E32\u0E19\u0E30\u0E40\u0E27\u0E47\u0E1A\u0E44\u0E0B\u0E15\
  \u0E4C."
title: "\u0E01\u0E32\u0E23\u0E14\u0E32\u0E27\u0E19\u0E4C\u0E42\u0E2B\u0E25\u0E14\u0E2B\
  \u0E19\u0E49\u0E32\u0E40\u0E27\u0E47\u0E1A"
weight: 42
---

## วิธีการ:
ใน Clojure, คุณสามารถใช้ `clj-http` เพื่อดาวน์โหลดเว็บเพจได้อย่างรวดเร็ว นี่คือตัวอย่างพื้นฐาน:

```Clojure
(require '[clj-http.client :as client])

(defn download-page [url]
  (client/get url))

;; ใช้มันแบบนี้:
(defn -main []
  (println (download-page "http://example.com")))
```

ถ้าคุณลองทำตาม, คุณจะได้รับแผนที่เต็มไปด้วยรายละเอียด ส่วนที่น่าสนใจอยู่ใต้ `:body` และ `:status`

## การดำดิ่งลึก
ในอดีต, การดาวน์โหลดเว็บเป็นการใช้ 'wget' หรือ 'curl' ที่บรรทัดคำสั่ง ตอนนี้, ภาษาอย่าง Clojure ทำให้เรื่องนี้เป็นเรื่องง่ายด้วยไลบรารี `clj-http` เป็นหนึ่งในไลบรารีเช่นนั้นที่ห่อหุ้มส่วนประกอบของ Apache HttpComponents ของ Java ให้เข้ากับสไตล์การเขียนฟังก์ชันของ Clojure

มีตัวเลือกอื่นไหม? แน่นอน คุณอาจเรียกใช้ `java.net.HttpURLConnection` โดยตรงหรือเลือกไลบรารีอื่นๆ เช่น `http-kit` – แต่ `clj-http` เป็นตัวเลือกที่สะดวกและมีส่วนประกอบส่วนใหญ่ที่คุณจะต้องการใช้งานอยู่แล้ว

เรื่องหลักๆ, `clj-http` จะเปลี่ยนคำขอของคุณเป็นเอนทิตี HTTP ของ Java, ทำการเรียก, และส่งคำตอบกลับมา ภายในตัวมัน, มันกำลังจัดการเรื่องการเปลี่ยนเส้นทาง, การวิเคราะห์ส่วนหัว, และการจัดการตัวเนื้อหาการตอบกลับเพื่อให้คุณสามารถโฟกัสไปที่ข้อมูลของคุณ, ไม่ใช่ระบบภายใน

## ดูเพิ่มเติม
- พื้นที่เก็บข้อมูล clj-http บน GitHub: [https://github.com/dakrone/clj-http](https://github.com/dakrone/clj-http)
- Clojure http-kit สำหรับวิธีการอื่น: [http://www.http-kit.org](http://www.http-kit.org)
- เว็บไซต์อย่างเป็นทางการของ Clojure เพื่อให้คุณเรียนรู้เพิ่มเติมเกี่ยวกับภาษา: [https://clojure.org](https://clojure.org)
