---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:50:31.812029-06:00
description: "\u0E01\u0E32\u0E23\u0E04\u0E49\u0E19\u0E2B\u0E32\u0E41\u0E25\u0E30\u0E41\
  \u0E17\u0E19\u0E17\u0E35\u0E48\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E0A\u0E48\
  \u0E27\u0E22\u0E43\u0E2B\u0E49\u0E04\u0E38\u0E13\u0E1E\u0E1A\u0E2A\u0E15\u0E23\u0E34\
  \u0E07\u0E17\u0E35\u0E48\u0E40\u0E09\u0E1E\u0E32\u0E30\u0E40\u0E08\u0E32\u0E30\u0E08\
  \u0E07\u0E43\u0E19\u0E40\u0E19\u0E37\u0E49\u0E2D\u0E2B\u0E32\u0E02\u0E49\u0E2D\u0E04\
  \u0E27\u0E32\u0E21 \u0E41\u0E25\u0E30\u0E2A\u0E25\u0E31\u0E1A\u0E21\u0E31\u0E19\u0E14\
  \u0E49\u0E27\u0E22\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E2D\u0E37\u0E48\u0E19 \u0E42\u0E1B\
  \u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E40\u0E0A\
  \u0E48\u0E19\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E01\u0E32\u0E23\u0E41\
  \u0E01\u0E49\u0E44\u0E02\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E23\u0E27\u0E14\u0E40\u0E23\
  \u0E47\u0E27\u2026"
lastmod: '2024-03-17T21:57:55.789884-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E04\u0E49\u0E19\u0E2B\u0E32\u0E41\u0E25\u0E30\u0E41\
  \u0E17\u0E19\u0E17\u0E35\u0E48\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E0A\u0E48\
  \u0E27\u0E22\u0E43\u0E2B\u0E49\u0E04\u0E38\u0E13\u0E1E\u0E1A\u0E2A\u0E15\u0E23\u0E34\
  \u0E07\u0E17\u0E35\u0E48\u0E40\u0E09\u0E1E\u0E32\u0E30\u0E40\u0E08\u0E32\u0E30\u0E08\
  \u0E07\u0E43\u0E19\u0E40\u0E19\u0E37\u0E49\u0E2D\u0E2B\u0E32\u0E02\u0E49\u0E2D\u0E04\
  \u0E27\u0E32\u0E21 \u0E41\u0E25\u0E30\u0E2A\u0E25\u0E31\u0E1A\u0E21\u0E31\u0E19\u0E14\
  \u0E49\u0E27\u0E22\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E2D\u0E37\u0E48\u0E19 \u0E42\u0E1B\
  \u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E40\u0E0A\
  \u0E48\u0E19\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E01\u0E32\u0E23\u0E41\
  \u0E01\u0E49\u0E44\u0E02\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E23\u0E27\u0E14\u0E40\u0E23\
  \u0E47\u0E27\u2026"
title: "\u0E01\u0E32\u0E23\u0E04\u0E49\u0E19\u0E2B\u0E32\u0E41\u0E25\u0E30\u0E41\u0E17\
  \u0E19\u0E17\u0E35\u0E48\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21"
weight: 10
---

## อะไร & ทำไม?

การค้นหาและแทนที่ข้อความช่วยให้คุณพบสตริงที่เฉพาะเจาะจงในเนื้อหาข้อความ และสลับมันด้วยอย่างอื่น โปรแกรมเมอร์ทำเช่นนี้เพื่อการแก้ไขอย่างรวดเร็ว การปรับโครงสร้างระบบขนาดใหญ่ หรือการประมวลผลข้อความโดยอัตโนมัติ มันเป็นเทคนิคการจัดการข้อความที่พื้นฐาน แต่ทรงพลัง

## วิธีการ:

ใน Clojure, เราใช้ฟังก์ชัน `clojure.string/replace` เพื่อค้นหาและแทนที่ข้อความ เรามาดูโค้ดกันเลย:

```clojure
(require '[clojure.string :as str])

;; การแทนที่พื้นฐาน
(str/replace "I like apples" "apples" "oranges")
;; => "I like oranges"

;; การใช้ regular expression เพื่อแทนที่สระทั้งหมด
(str/replace "Hello, World!" "[AEIOUaeiou]" "*")
;; => "H*ll*, W*rld!"

;; การแทนที่ด้วยฟังก์ชันสำหรับการเปลี่ยนแปลงแบบไดนามิก
(str/replace "I have 2 apples and 5 bananas"
             #"\d+"
             (fn [match] (str (inc (Integer/parseInt match)))))
;; => "I have 3 apples and 6 bananas"
```

ง่ายเหมือนนั้นแหละ รันมัน แล้วคุณจะเห็นการเปลี่ยนแปลงเหล่านั้นใน REPL ของคุณเลย

## การศึกษาลึก

การค้นหาและแทนที่ในข้อความไม่ใช่เรื่องใหม่ มันเก่าแก่ในการคอมพิวเตอร์ คุณได้รับมันมาจากตัวแก้ไขเบื้องต้นเช่น `sed` ใน Unix เราได้พัฒนามาไกลแล้วตั้งแต่นั้น

Clojure, ที่อยู่บน JVM, หมายความว่าคุณมีพลังการจับคู่สายอักขระแบบ Java ให้ใช้งาน ด้านประสิทธิภาพ, มันดีสำหรับสคริปต์เล็กๆ แต่จำไว้, การใช้มากเกินไปในการประมวลผลข้อความขนาดใหญ่อาจทำให้ประสิทธิภาพลดลง

สำหรับทางเลือกอื่น, นอกจาก `clojure.string/replace`, ยังมีห้องสมุดตาม regular expression หรือแม้กระทั่งเขียนฟังก์ชันกำหนดเองของคุณเองหากคุณรู้สึกชอบผจญภัย พิจารณา `replace-first` หากคุณต้องการการเปลี่ยนแปลงที่เกิดขึ้นครั้งเดียว

ในด้านการใช้งาน, วิธีการของ Clojure ที่เน้นความไม่สามารถเปลี่ยนแปลงได้หมายถึงการแทนที่แต่ละครั้งจะส่งผลให้เกิดสายอักขระใหม่ ไม่มีสายอักขระที่เปลี่ยนแปลงได้หมายถึงความบกพร่องและความประหลาดใจน้อยลง

## ดูเพิ่มเติม

เพื่อการศึกษาลึกขึ้น, ลองดูทรัพยากรเหล่านี้:

- คู่มือ `clojure.string` ของ Clojure [API documentation](https://clojuredocs.org/clojure.string/replace)
- เกี่ยวกับ regular expressions, จาก [Pattern class](https://docs.oracle.com/javase/7/docs/api/java/util/regex/Pattern.html) ของ Java
