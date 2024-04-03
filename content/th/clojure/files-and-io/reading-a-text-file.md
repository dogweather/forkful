---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:49:30.609853-06:00
description: "\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E44\u0E1F\u0E25\u0E4C\u0E02\
  \u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\
  \u0E32\u0E23\u0E14\u0E36\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E08\u0E32\u0E01\
  \u0E44\u0E1F\u0E25\u0E4C\u0E17\u0E35\u0E48\u0E40\u0E01\u0E47\u0E1A\u0E2D\u0E22\u0E39\
  \u0E48\u0E1A\u0E19\u0E14\u0E34\u0E2A\u0E01\u0E4C\u0E40\u0E02\u0E49\u0E32\u0E2A\u0E39\
  \u0E48\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13\
  \u2026"
lastmod: '2024-03-17T21:57:55.826174-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E44\u0E1F\u0E25\u0E4C\u0E02\
  \u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\
  \u0E32\u0E23\u0E14\u0E36\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E08\u0E32\u0E01\
  \u0E44\u0E1F\u0E25\u0E4C\u0E17\u0E35\u0E48\u0E40\u0E01\u0E47\u0E1A\u0E2D\u0E22\u0E39\
  \u0E48\u0E1A\u0E19\u0E14\u0E34\u0E2A\u0E01\u0E4C\u0E40\u0E02\u0E49\u0E32\u0E2A\u0E39\
  \u0E48\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13\
  \ \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\
  \u0E33\u0E40\u0E0A\u0E48\u0E19\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E1B\
  \u0E23\u0E30\u0E21\u0E27\u0E25\u0E1C\u0E25\u0E2B\u0E23\u0E37\u0E2D\u0E27\u0E34\u0E40\
  \u0E04\u0E23\u0E32\u0E30\u0E2B\u0E4C\u0E40\u0E19\u0E37\u0E49\u0E2D\u0E2B\u0E32\u0E42\
  \u0E14\u0E22\u0E44\u0E21\u0E48\u0E15\u0E49\u0E2D\u0E07\u0E1B\u0E49\u0E2D\u0E19\u0E02\
  \u0E49\u0E2D\u0E21\u0E39\u0E25\u0E14\u0E49\u0E27\u0E22\u0E21\u0E37\u0E2D, \u0E17\
  \u0E33\u0E07\u0E32\u0E19\u0E2D\u0E31\u0E15\u0E42\u0E19\u0E21\u0E31\u0E15\u0E34,\
  \ \u0E2B\u0E23\u0E37\u0E2D\u0E41\u0E22\u0E01\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\
  \u0E30\u0E2B\u0E4C\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E01\u0E32\u0E23\u0E15\u0E31\
  \u0E49\u0E07\u0E04\u0E48\u0E32."
title: "\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E44\u0E1F\u0E25\u0E4C\u0E02\u0E49\
  \u0E2D\u0E04\u0E27\u0E32\u0E21"
weight: 22
---

## วิธีการ:
```Clojure
;; อ่านไฟล์ทั้งหมดเป็นสตริง
(slurp "example.txt")

;; ผลลัพธ์: "สวัสดี, นี่คือเนื้อหาไฟล์ของคุณ!"

;; อ่านไฟล์เป็นบรรทัดๆ
(with-open [reader (clojure.java.io/reader "example.txt")]
  (doseq [line (line-seq reader)]
    (println line)))

;; ผลลัพธ์:
;; สวัสดี,
;; นี่คือ
;; เนื้อหาไฟล์ของคุณ!
```

## ลงลึก
โดยทั่วไป, การอ่านไฟล์ในภาษาโปรแกรมมิ่งถือว่าเป็นงานที่ต้องเขียนมากมีขั้นตอนมากมายในการจัดการข้อผิดพลาดและทรัพยากร ใน Clojure, คุณได้รับประโยชน์จากฟังก์ชัน `slurp`, บรรทัดเดียวที่สวยงามเพื่อคว้าเนื้อหาไฟล์ทั้งหมด สำหรับการอ่านแบบบรรทัดต่อบรรทัด, `line-seq` ร่วมกับ `with-open` ช่วยให้มั่นใจได้ว่าการจัดการไฟล์เป็นไปอย่างมีประสิทธิภาพและปลอดภัย เป็นที่ควรกล่าวถึงด้วยว่าแม้ `slurp` จะสะดวก แต่ไม่เหมาะกับไฟล์ขนาดใหญ่เนื่องจากข้อจำกัดด้านหน่วยความจำ นั่นคือเมื่อ `line-seq` โดดเด่น, เนื่องจากมันอ่านไฟล์อย่างเฉื่อยชา, บรรทัดต่อบรรทัด

ทางเลือกอื่นๆ ในการอ่านไฟล์ใน Clojure รวมถึงการใช้ `clojure.java.io/file` กับฟังก์ชันเช่น `reader` และโครงสร้างเช่น `with-open` เพื่อจัดการจับไฟล์ด้วยตัวเอง การตัดสินใจที่นี่คือระหว่างการใช้งานง่าย (`slurp`) กับการควบคุมอย่างละเอียดประกอบด้วยความปลอดภัยทรัพยากร (`with-open` และ `reader`)

ในทางของการนำไปใช้งาน, วิธีการของ Clojure มีพื้นฐานมาจากคลาส IO ของ Java, ดังนั้นเมื่อคุณกำลังจัดการกับไฟล์ใน Clojure, คุณกำลังจัดการกับคลังสมบัติที่ถูกทดสอบอย่างดีของ Java, ที่ถูกห่อหุ้มในรูปแบบการทำงานแบบฟังก์ชัน ควรเฝ้าระวังทรัพยากรเสมอ: ไฟล์ที่เปิดใช้งานใช้จับมือและหน่วยความจำ, ดังนั้นการจัดการไฟล์อย่างเหมาะสมเป็นนิสัยที่ดี

## ดูเพิ่มเติม
- ClojureDocs สำหรับ `slurp`: https://clojuredocs.org/clojure.core/slurp
- ClojureDocs สำหรับ `line-seq`: https://clojuredocs.org/clojure.core/line-seq
- การทำงานร่วมกันของ Java ใน Clojure: https://clojure.org/reference/java_interop
- การทำงานกับไฟล์ใน Clojure (Practical.li): https://practical.li/clojure/working-with-files.html
