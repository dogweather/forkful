---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:49:39.657470-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E43\u0E19 Clojure, \u0E04\
  \u0E38\u0E13\u0E21\u0E31\u0E01\u0E08\u0E30\u0E1E\u0E34\u0E21\u0E1E\u0E4C\u0E02\u0E49\
  \u0E2D\u0E21\u0E39\u0E25\u0E01\u0E32\u0E23\u0E14\u0E35\u0E1A\u0E31\u0E01\u0E42\u0E14\
  \u0E22\u0E43\u0E0A\u0E49 `println`, `printf`, `pr`, \u0E2B\u0E23\u0E37\u0E2D `prn`\
  \ \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E27\u0E34\u0E18\u0E35\u0E17\u0E35\u0E48\
  \u0E04\u0E38\u0E13\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E40\u0E1E\u0E34\u0E48\u0E21\
  \u0E01\u0E32\u0E23\u0E1E\u0E34\u0E21\u0E1E\u0E4C\u0E14\u0E35\u0E1A\u0E31\u0E01\u0E44\
  \u0E14\u0E49."
lastmod: '2024-03-17T21:57:55.810705-06:00'
model: gpt-4-0125-preview
summary: "\u0E43\u0E19 Clojure, \u0E04\u0E38\u0E13\u0E21\u0E31\u0E01\u0E08\u0E30\u0E1E\
  \u0E34\u0E21\u0E1E\u0E4C\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E01\u0E32\u0E23\u0E14\
  \u0E35\u0E1A\u0E31\u0E01\u0E42\u0E14\u0E22\u0E43\u0E0A\u0E49 `println`, `printf`,\
  \ `pr`, \u0E2B\u0E23\u0E37\u0E2D `prn` \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E27\
  \u0E34\u0E18\u0E35\u0E17\u0E35\u0E48\u0E04\u0E38\u0E13\u0E2A\u0E32\u0E21\u0E32\u0E23\
  \u0E16\u0E40\u0E1E\u0E34\u0E48\u0E21\u0E01\u0E32\u0E23\u0E1E\u0E34\u0E21\u0E1E\u0E4C\
  \u0E14\u0E35\u0E1A\u0E31\u0E01\u0E44\u0E14\u0E49."
title: "\u0E01\u0E32\u0E23\u0E1E\u0E34\u0E21\u0E1E\u0E4C\u0E1C\u0E25\u0E25\u0E31\u0E1E\
  \u0E18\u0E4C\u0E01\u0E32\u0E23\u0E41\u0E01\u0E49\u0E44\u0E02\u0E42\u0E04\u0E49\u0E14"
weight: 33
---

## วิธีการ:
ใน Clojure, คุณมักจะพิมพ์ข้อมูลการดีบักโดยใช้ `println`, `printf`, `pr`, หรือ `prn` นี่คือวิธีที่คุณสามารถเพิ่มการพิมพ์ดีบักได้:

```Clojure
(defn add-and-print [a b]
  (println "Adding:" a "and" b) ; พิมพ์การดำเนินการ
  (let [result (+ a b)]
    (println "Result:" result)  ; พิมพ์ผลลัพธ์
    result))                    ; คืนค่าผลลัพธ์

(add-and-print 3 4)
```
ผลลัพธ์ตัวอย่าง:
```
Adding: 3 and 4
Result: 7
```

หรือ, สำหรับการดีบักค่าในขณะที่อยู่กลางของ macro การเรียง:

```Clojure
(require '[clojure.pprint :refer [pprint]])

(-> 3
    (+ 5)
    (pprint)             ; พิมพ์ผลลัพธ์กลาง
    (* 2))
```
ผลลัพธ์ตัวอย่าง:
```
8
```

## ลงลึก:
การพิมพ์การดีบักมีประวัติยาวนาน อาจจะเก่าเท่ากับการเขียนโปรแกรมเอง เป็นสิ่งที่ตรงไปตรงมา: คุณใส่คำสั่งพิมพ์ในที่ที่คุณสงสัยว่าอาจมีปัญหา รันโค้ด และดูผลลัพธ์

ฟังก์ชั่นของ Clojure สำหรับการพิมพ์ดีบักค่อนข้างคล้ายกับภาษา Lisp อื่นๆ แต่มีรสชาติฟังก์ชันโดยทั่วไป `println` และ `prn` แตกต่างกันตรงที่หลังเขียนข้อมูลในรูปแบบที่สามารถอ่านได้โดย Clojure reader `pprint` (พิมพ์สวย) จาก `clojure.pprint` สามารถใช้เมื่อคุณต้องการรูปแบบที่ดูดีกว่า

เครื่องมือเฉพาะของ Clojure สำหรับการดีบักคือ `tap>` ที่นำมาใช้ใน Clojure 1.10, มันอนุญาตให้ "แตะ" เข้าไปในโค้ดที่กำลังทำงานอย่างไม่สะดุด โดยไม่ต้องมีคำสั่งพิมพ์ในโค้ดของคุณ

สำหรับโปรเจคที่ใหญ่ขึ้นหรือมีความซับซ้อนมากขึ้น พิจารณาใช้ไลบรารีการบันทึกข้อมูลเช่น `clojure.tools.logging` หรือ `timbre`

## ดูเพิ่มเติม:
- คลังข้อมูล GitHub ของ [`clojure.tools.logging`](https://github.com/clojure/tools.logging)
- คลังข้อมูล GitHub ของไลบรารีการบันทึกข้อมูล [Timbre](https://github.com/ptaoussanis/timbre)
- คู่มือ [`clojure.pprint`](https://clojuredocs.org/clojure.pprint/pprint) บน ClojureDocs
