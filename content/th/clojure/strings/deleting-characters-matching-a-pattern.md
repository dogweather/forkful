---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:46:09.244532-06:00
description: "\u0E01\u0E32\u0E23\u0E25\u0E1A\u0E2D\u0E31\u0E01\u0E02\u0E23\u0E30\u0E17\
  \u0E35\u0E48\u0E15\u0E23\u0E07\u0E01\u0E31\u0E1A\u0E41\u0E1E\u0E17\u0E40\u0E17\u0E34\
  \u0E23\u0E4C\u0E19\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E17\
  \u0E33\u0E04\u0E27\u0E32\u0E21\u0E2A\u0E30\u0E2D\u0E32\u0E14\u0E25\u0E33\u0E14\u0E31\
  \u0E1A\u0E02\u0E2D\u0E07\u0E2D\u0E31\u0E01\u0E02\u0E23\u0E30\u0E43\u0E14\u0E46 \u0E2D\
  \u0E2D\u0E01\u0E08\u0E32\u0E01\u0E2A\u0E15\u0E23\u0E34\u0E07 \u0E40\u0E2B\u0E25\u0E48\
  \u0E32\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\
  \u0E33\u0E40\u0E0A\u0E48\u0E19\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E17\
  \u0E33\u0E04\u0E27\u0E32\u0E21\u0E2A\u0E30\u0E2D\u0E32\u0E14\u0E02\u0E49\u0E2D\u0E21\
  \u0E39\u0E25, \u0E1A\u0E31\u0E07\u0E04\u0E31\u0E1A\u0E43\u0E0A\u0E49\u0E23\u0E39\
  \u0E1B\u0E41\u0E1A\u0E1A,\u2026"
lastmod: '2024-03-17T21:57:55.788818-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E25\u0E1A\u0E2D\u0E31\u0E01\u0E02\u0E23\u0E30\u0E17\
  \u0E35\u0E48\u0E15\u0E23\u0E07\u0E01\u0E31\u0E1A\u0E41\u0E1E\u0E17\u0E40\u0E17\u0E34\
  \u0E23\u0E4C\u0E19\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E17\
  \u0E33\u0E04\u0E27\u0E32\u0E21\u0E2A\u0E30\u0E2D\u0E32\u0E14\u0E25\u0E33\u0E14\u0E31\
  \u0E1A\u0E02\u0E2D\u0E07\u0E2D\u0E31\u0E01\u0E02\u0E23\u0E30\u0E43\u0E14\u0E46 \u0E2D\
  \u0E2D\u0E01\u0E08\u0E32\u0E01\u0E2A\u0E15\u0E23\u0E34\u0E07 \u0E40\u0E2B\u0E25\u0E48\
  \u0E32\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\
  \u0E33\u0E40\u0E0A\u0E48\u0E19\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E17\
  \u0E33\u0E04\u0E27\u0E32\u0E21\u0E2A\u0E30\u0E2D\u0E32\u0E14\u0E02\u0E49\u0E2D\u0E21\
  \u0E39\u0E25, \u0E1A\u0E31\u0E07\u0E04\u0E31\u0E1A\u0E43\u0E0A\u0E49\u0E23\u0E39\
  \u0E1B\u0E41\u0E1A\u0E1A,\u2026"
title: "\u0E01\u0E32\u0E23\u0E25\u0E1A\u0E15\u0E31\u0E27\u0E2D\u0E31\u0E01\u0E29\u0E23\
  \u0E17\u0E35\u0E48\u0E15\u0E23\u0E07\u0E01\u0E31\u0E1A\u0E23\u0E39\u0E1B\u0E41\u0E1A\
  \u0E1A"
---

{{< edit_this_page >}}

## อะไร & ทำไม?
การลบอักขระที่ตรงกับแพทเทิร์นหมายถึงการทำความสะอาดลำดับของอักขระใดๆ ออกจากสตริง เหล่าโปรแกรมเมอร์ทำเช่นนี้เพื่อทำความสะอาดข้อมูล, บังคับใช้รูปแบบ, หรือลบข้อมูลที่ไม่ต้องการ.

## วิธีการ:

ในการลบอักขระโดยใช้แพทเทิร์นใน Clojure, คุณใช้ regular expressions ร่วมกับฟังก์ชัน `re-seq`, `re-find`, หรือ `re-matches` และคู่กับ `clojure.string/replace`.

```Clojure
(require '[clojure.string :as str])

;; ลบตัวเลขทั้งหมดออกจากสตริง
(str/replace "He110 W0rld" #"\d+" "")
;; => "He Wrd"

;; ลบอักขระพิเศษที่เจาะจง
(str/replace "Hello, World! #Clojure" #"[,!#]" "")
;; => "Hello World Clojure"

;; เก็บเฉพาะอักขระของคำและช่องว่าง
(str/replace "Email@Example.com" #"[^\w\s]+" "")
;; => "EmailExamplecom"
```

## ลงลึก
Clojure, สะท้อนมรดกจาก Lisp, โดดเด่นในการประมวลผลสัญลักษณ์, ทำให้การจับคู่พาทเทิร์นง่ายขึ้น นำเสนอในปี 2007, มันสร้างต่อความสามารถของ Java Virtual Machine (JVM), ใช้ประโยชน์จาก `Pattern` class ของ Java สำหรับ regular expressions.

ทางเลือกแทนการใช้ regex รวมถึงการไต่สวนและจัดการสตริงด้วยมือ, แต่เหล่านี้มักจะเพิ่มความยาวและเสี่ยงต่อข้อผิดพลาดมากขึ้น ไลบรารีเช่น `clojure.spec` สามารถช่วยในการตรวจสอบและจัดรูปแบบข้อมูลตามแพทเทิร์นโดยไม่ต้องลบโดยตรง

การดำเนินการลบมักจะมีประสิทธิภาพสูง, แต่ต้องระมัดระวังเรื่องความซับซ้อนของ regex, ซึ่งอาจทำให้งาน O(n) เป็นเลวทรามลง สตริงที่ไม่สามารถเปลี่ยนแปลงได้ใน Clojure หมายความว่าการทำ `replace` ในทุกครั้งจะสร้างสตริงใหม่, ซึ่งควรพิจารณาสำหรับแอปพลิเคชันที่มีความไวต่อหน่วยความจำ

## ดูเพิ่ม
- [API สตริงของ Clojure](https://clojure.github.io/clojure/clojure.string-api.html)
- [Java Pattern class](https://docs.oracle.com/javase/7/docs/api/java/util/regex/Pattern.html)
- [Regular-Expressions.info](https://www.regular-expressions.info/)
- [clojure.spec](https://clojure.org/guides/spec)
