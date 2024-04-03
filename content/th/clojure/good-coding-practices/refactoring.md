---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:50:00.988804-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E01\u0E32\u0E23 Refactor\
  \ \u0E43\u0E19 Clojure\u2014\u0E02\u0E2D\u0E1A\u0E04\u0E38\u0E13\u0E2A\u0E33\u0E2B\
  \u0E23\u0E31\u0E1A syntax \u0E17\u0E35\u0E48\u0E2A\u0E30\u0E2D\u0E32\u0E14\u0E41\
  \u0E25\u0E30\u0E41\u0E1A\u0E1A\u0E08\u0E33\u0E25\u0E2D\u0E07\u0E01\u0E32\u0E23\u0E17\
  \u0E33\u0E07\u0E32\u0E19\u0E41\u0E1A\u0E1A\u0E1F\u0E31\u0E07\u0E01\u0E4C\u0E0A\u0E31\
  \u0E19\u2014\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E17\u0E33\u0E44\u0E14\u0E49\u0E2D\
  \u0E22\u0E48\u0E32\u0E07\u0E15\u0E23\u0E07\u0E44\u0E1B\u0E15\u0E23\u0E07\u0E21\u0E32\
  \u0E2D\u0E22\u0E48\u0E32\u0E07\u0E44\u0E21\u0E48\u0E19\u0E48\u0E32\u0E40\u0E0A\u0E37\
  \u0E48\u0E2D\u2026"
lastmod: '2024-03-17T21:57:55.816469-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23 Refactor \u0E43\u0E19 Clojure\u2014\u0E02\u0E2D\u0E1A\
  \u0E04\u0E38\u0E13\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A syntax \u0E17\u0E35\u0E48\
  \u0E2A\u0E30\u0E2D\u0E32\u0E14\u0E41\u0E25\u0E30\u0E41\u0E1A\u0E1A\u0E08\u0E33\u0E25\
  \u0E2D\u0E07\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E41\u0E1A\u0E1A\u0E1F\
  \u0E31\u0E07\u0E01\u0E4C\u0E0A\u0E31\u0E19\u2014\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\
  \u0E17\u0E33\u0E44\u0E14\u0E49\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E15\u0E23\u0E07\u0E44\
  \u0E1B\u0E15\u0E23\u0E07\u0E21\u0E32\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E44\u0E21\u0E48\
  \u0E19\u0E48\u0E32\u0E40\u0E0A\u0E37\u0E48\u0E2D \u0E21\u0E32\u0E25\u0E2D\u0E07\u0E08\
  \u0E31\u0E14\u0E01\u0E32\u0E23\u0E01\u0E31\u0E1A\u0E2A\u0E16\u0E32\u0E19\u0E01\u0E32\
  \u0E23\u0E13\u0E4C\u0E17\u0E35\u0E48\u0E1E\u0E1A\u0E1A\u0E48\u0E2D\u0E22."
title: "\u0E01\u0E32\u0E23\u0E1B\u0E23\u0E31\u0E1A\u0E42\u0E04\u0E23\u0E07\u0E2A\u0E23\
  \u0E49\u0E32\u0E07\u0E42\u0E04\u0E49\u0E14"
weight: 19
---

## วิธีการ:
การ Refactor ใน Clojure—ขอบคุณสำหรับ syntax ที่สะอาดและแบบจำลองการทำงานแบบฟังก์ชัน—สามารถทำได้อย่างตรงไปตรงมาอย่างไม่น่าเชื่อ มาลองจัดการกับสถานการณ์ที่พบบ่อย: การวนซ้ำเหนือคอลเลกชัน คุณอาจเริ่มต้นด้วยลูป `for` ดังนี้:

```clojure
(defn calculate-sum [numbers]
  (reduce + 0 numbers))

(defn old-way []
  (let [nums (range 1 11)]
    (calculate-sum nums)))
```

การเรียก `(old-way)` จะให้ผลรวมของเรา 55, ผลรวมจาก 1 ถึง 10 แต่เฮ้ เราสามารถ Refactor นี้ให้เป็นสไตล์ของ Clojure มากขึ้นได้:

```clojure
(defn new-way []
  (->> (range 1 11)
       (reduce +)))
```

ฟังก์ชัน `(new-way)` ที่ถูก Refactor ใช้แมโครการทำงานร่วมกันในการผ่านช่วงตรงไปยัง `reduce` ทำให้การทำงานมีประสิทธิภาพมากขึ้น

## การศึกษาลึก
ศิลปะของการ Refactor มีรากฐานมาจากยุคแรก ๆ ของการพัฒนาซอฟต์แวร์แต่ได้รับความนิยมอย่างมากกับหนังสือเรื่อง "Refactoring: Improving the Design of Existing Code" ที่เขียนโดย Martin Fowler ที่ตีพิมพ์ในปี 1999 ใน Clojure, Refactor มักจะสนับสนุนหลักการของการเขียนโปรแกรมแบบฟังก์ชันโดยเฉพาะอย่างยิ่งการใช้ฟังก์ชันที่บริสุทธิ์และโครงสร้างข้อมูลที่ไม่สามารถเปลี่ยนแปลงได้

การเลือกที่ไม่ต้อง Refactor ด้วยตนเองใน Clojure อาจรวมถึงการใช้เครื่องมือเช่น Cursive, ปลั๊กอินของ IntelliJ IDEA ที่ได้รับความนิยมซึ่งเสนอการ Refactor อัตโนมัติที่เฉพาะเจาะจงกับ Clojure ยังมี clj-refactor, แพ็กเกจสำหรับ Emacs สำหรับ Clojure ที่ให้ชุดฟังก์ชันสำหรับ Refactor

ความท้าทายที่เฉพาะเจาะจงต่อการ Refactor ใน Clojure คือการจัดการกับสถานะและผลกระทบข้างเคียงในรูปแบบที่หลักการไม่สามารถเปลี่ยนแปลงและไม่มีผลกระทบข้างเคียง การใช้ atoms, refs, agents และ transients อย่างระมัดระวังเป็นสิ่งสำคัญในการรักษาทั้งประสิทธิภาพและความถูกต้องในระหว่างการ Refactor

## ดูเพิ่มเติม
- "Refactoring: Improving the Design of Existing Code" ของ Martin Fowler สำหรับแนวคิดพื้นฐาน
- [Clojure Docs](https://clojuredocs.org/) สำหรับตัวอย่างเฉพาะของโค้ด Clojure ที่เป็นสัจธรรม
- [clj-refactor](https://github.com/clojure-emacs/clj-refactor.el) สำหรับการปรับปรุงทำให้เป็นอัตโนมัติใน Emacs
- [Cursive](https://cursive-ide.com/) สำหรับผู้ใช้ IntelliJ ที่มองหาช่วยเหลือในการ Refactor แบบอัตโนมัติ
- [Refactoring with Rich Hickey](https://www.infoq.com/presentations/Simple-Made-Easy/) - การบรรยายโดยผู้สร้าง Clojure ที่ให้ข้อมูลเชิงลึกเกี่ยวกับปรัชญา Clojure ซึ่งสามารถแนะนำการตัดสินใจการ Refactor ได้อย่างมีประสิทธิผล
