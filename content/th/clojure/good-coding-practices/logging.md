---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:48:36.823024-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E17\u0E33: Clojure \u0E43\u0E0A\u0E49\u0E1B\
  \u0E23\u0E30\u0E42\u0E22\u0E0A\u0E19\u0E4C\u0E08\u0E32\u0E01\u0E1F\u0E31\u0E07\u0E01\
  \u0E4C\u0E0A\u0E31\u0E19\u0E01\u0E32\u0E23\u0E1A\u0E31\u0E19\u0E17\u0E36\u0E01\u0E02\
  \u0E2D\u0E07 Java, \u0E41\u0E15\u0E48\u0E04\u0E38\u0E13\u0E2A\u0E32\u0E21\u0E32\u0E23\
  \u0E16\u0E40\u0E02\u0E49\u0E32\u0E16\u0E36\u0E07\u0E44\u0E14\u0E49\u0E43\u0E19\u0E27\
  \u0E34\u0E18\u0E35\u0E17\u0E35\u0E48\u0E40\u0E1B\u0E47\u0E19 Clojure \u0E21\u0E32\
  \u0E01\u0E02\u0E36\u0E49\u0E19 \u0E25\u0E2D\u0E07\u0E14\u0E39\u0E27\u0E48\u0E32\u0E04\
  \u0E38\u0E13\u0E2D\u0E32\u0E08\u0E43\u0E0A\u0E49 `clojure.tools.logging`\u2026"
lastmod: '2024-03-17T21:57:55.814402-06:00'
model: gpt-4-0125-preview
summary: "Clojure \u0E43\u0E0A\u0E49\u0E1B\u0E23\u0E30\u0E42\u0E22\u0E0A\u0E19\u0E4C\
  \u0E08\u0E32\u0E01\u0E1F\u0E31\u0E07\u0E01\u0E4C\u0E0A\u0E31\u0E19\u0E01\u0E32\u0E23\
  \u0E1A\u0E31\u0E19\u0E17\u0E36\u0E01\u0E02\u0E2D\u0E07 Java, \u0E41\u0E15\u0E48\u0E04\
  \u0E38\u0E13\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E40\u0E02\u0E49\u0E32\u0E16\u0E36\
  \u0E07\u0E44\u0E14\u0E49\u0E43\u0E19\u0E27\u0E34\u0E18\u0E35\u0E17\u0E35\u0E48\u0E40\
  \u0E1B\u0E47\u0E19 Clojure \u0E21\u0E32\u0E01\u0E02\u0E36\u0E49\u0E19 \u0E25\u0E2D\
  \u0E07\u0E14\u0E39\u0E27\u0E48\u0E32\u0E04\u0E38\u0E13\u0E2D\u0E32\u0E08\u0E43\u0E0A\
  \u0E49 `clojure.tools.logging` \u0E0B\u0E36\u0E48\u0E07\u0E43\u0E2B\u0E49\u0E01\u0E32\
  \u0E23\u0E41\u0E1B\u0E25\u0E07\u0E17\u0E35\u0E48\u0E40\u0E23\u0E35\u0E22\u0E1A\u0E07\
  \u0E48\u0E32\u0E22\u0E01\u0E27\u0E48\u0E32\u0E2B\u0E25\u0E32\u0E22\u0E01\u0E23\u0E2D\
  \u0E1A\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E32\u0E23\u0E1A\u0E31\
  \u0E19\u0E17\u0E36\u0E01."
title: "\u0E01\u0E32\u0E23\u0E1A\u0E31\u0E19\u0E17\u0E36\u0E01\u0E25\u0E47\u0E2D\u0E01"
weight: 17
---

## วิธีทำ:
Clojure ใช้ประโยชน์จากฟังก์ชันการบันทึกของ Java, แต่คุณสามารถเข้าถึงได้ในวิธีที่เป็น Clojure มากขึ้น ลองดูว่าคุณอาจใช้ `clojure.tools.logging` ซึ่งให้การแปลงที่เรียบง่ายกว่าหลายกรอบการทำงานการบันทึก:

ประการแรก, เพิ่มการพึ่งพา `clojure.tools.logging` และการใช้งานการบันทึกเช่น `log4j` ในไฟล์ `project.clj` ของคุณ:

```clojure
:dependencies [[org.clojure/clojure "1.10.3"]
               [org.clojure/tools.logging "1.1.0"]
               [log4j/log4j "1.2.17"]]
```

ตอนนี้, ลองบันทึกข้อความบางส่วน:

```clojure
(require '[clojure.tools.logging :as log])

(defn compute-answer-to-everything []
  (log/debug "เริ่มการคำนวณที่รุนแรง...")
  (Thread/sleep 3000) ; จำลองการคำนวณนาน
  (log/info "การคำนวณเสร็จสิ้น คำตอบคือ 42.")
  42)

(compute-answer-to-everything)
```
ผลลัพธ์ไม่จะแสดงข้อความ `DEBUG` โดยปริยาย เนื่องจากระดับการบันทึกมักถูกตั้งค่าเป็น `INFO`:

```
INFO  [your-namespace] - การคำนวณเสร็จสิ้น คำตอบคือ 42.
```

คุณสามารถกำหนดค่าสำหรับระดับการบันทึกและ appenders ในไฟล์ `log4j.properties` เพื่อรับผลลัพธ์ที่ละเอียดยิ่งขึ้นหากต้องการ

## ศึกษาลึกลงไป
`clojure.tools.logging` ของ Clojure มีมานานแล้วและทำหน้าที่เป็นสะพานระหว่างโค้ด Clojure กับโลกการบันทึกของ Java ตลอดประวัติศาสตร์ Java ได้ผ่านการเปลี่ยนแปลงหลายครั้งและไลบรารีสำหรับการบันทึก เช่น API การบันทึกที่มีอยู่ใน Java, `log4j`, `slf4j`, และ `logback`

ใน Clojure, ในขณะที่คุณสามารถใช้กรอบการทำงานการบันทึกของ Java ได้โดยตรง `clojure.tools.logging` จะตรวจสอบและมอบหมายไปยังกรอบการทำงานการบันทึกที่พบใน classpath ของคุณ ช่วยให้คุณไม่ต้องผูกติดกับการใช้งานเฉพาะ ซึ่งสามารถช่วยให้โค้ด Clojure ของคุณเป็นแบบพกพาและมีโมดูลาร์มากขึ้น

ทางเลือกที่ `clojure.tools.logging` ภายในระบบนิเวศ Clojure รวมถึงไลบรารีเช่น `timbre`, ซึ่งเป็นไลบรารีการบันทึกของ Clojure ที่แท้จริงด้วยคุณสมบัติเช่น การหมุนการบันทึก, การกรอง, และการบันทึกแบบอสิงค์โครนัส

รายละเอียดการใช้งานเป็นเรื่องสำคัญเมื่อมาถึงการบันทึกในสภาพแวดล้อมหลายเธรดเช่น Clojure ที่นี่, ความไม่สามารถเปลี่ยนแปลง และการจัดการผลข้างเคียงมีข้อได้เปรียบที่ชัดเจน การบันทึก, เป็นผลข้างเคียง, ควรได้รับความระมัดระวังเพื่อหลีกเลี่ยงจุดอับปางของประสิทธิภาพและรับรองความปลอดภัยของเธรด ซึ่งกรอบการทำงานการบันทึกของ Java มักจะดูแลให้

สุดท้าย, พิจารณาการบันทึกเป็นโครงสร้างข้อมูล ที่บันทึกเป็นข้อมูลโครงสร้าง (เช่น JSON) นี่อาจมีประโยชน์อย่างมากสำหรับการวิเคราะห์และประมวลผลในภายหลัง โดยเฉพาะเมื่อจัดการกับระบบที่กระจายขนาดใหญ่

## ดูเพิ่มเติม
หากคุณต้องการเรียนรู้เพิ่มเติม ให้พิจารณาตรวจสอบทรัพยากรเหล่านี้:

- เอกสารการใช้งาน Clojure Tools Logging: https://github.com/clojure/tools.logging
- Timbre, ไลบรารีการบันทึกของ Clojure: https://github.com/ptaoussanis/timbre
- การกำหนดค่า Log4J ใน Clojure: http://clojure-doc.org/articles/tutorials/logging_with_log4j.html
- คู่มือ Logback สำหรับการตั้งค่าขั้นสูง: http://logback.qos.ch/manual/
- คู่มือการบันทึกโครงสร้างใน Clojure: https://corfield.org/blog/2020/04/28/structured-logging/
