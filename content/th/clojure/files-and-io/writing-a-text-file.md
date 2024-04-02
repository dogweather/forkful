---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:53.459143-06:00
description: "\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E44\u0E1F\u0E25\u0E4C\
  \u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E43\u0E19 Clojure \u0E2B\u0E21\u0E32\
  \u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E2B\u0E23\
  \u0E37\u0E2D\u0E41\u0E01\u0E49\u0E44\u0E02\u0E44\u0E1F\u0E25\u0E4C\u0E40\u0E1E\u0E37\
  \u0E48\u0E2D\u0E1A\u0E31\u0E19\u0E17\u0E36\u0E01\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\
  \u0E19\u0E2D\u0E01\u0E41\u0E2D\u0E1B\u0E1E\u0E25\u0E34\u0E40\u0E04\u0E0A\u0E31\u0E19\
  \u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13 \u0E0B\u0E36\u0E48\u0E07\u0E17\u0E33\u0E43\u0E2B\
  \u0E49\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E23\u0E31\u0E01\u0E29\u0E32\u0E02\u0E49\
  \u0E2D\u0E21\u0E39\u0E25, \u0E01\u0E32\u0E23\u0E01\u0E33\u0E2B\u0E19\u0E14\u0E04\
  \u0E48\u0E32, \u0E01\u0E32\u0E23\u0E1A\u0E31\u0E19\u0E17\u0E36\u0E01\u0E23\u0E32\
  \u0E22\u0E01\u0E32\u0E23,\u2026"
lastmod: '2024-03-17T21:57:55.827019-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E44\u0E1F\u0E25\u0E4C\
  \u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E43\u0E19 Clojure \u0E2B\u0E21\u0E32\
  \u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E2B\u0E23\
  \u0E37\u0E2D\u0E41\u0E01\u0E49\u0E44\u0E02\u0E44\u0E1F\u0E25\u0E4C\u0E40\u0E1E\u0E37\
  \u0E48\u0E2D\u0E1A\u0E31\u0E19\u0E17\u0E36\u0E01\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\
  \u0E19\u0E2D\u0E01\u0E41\u0E2D\u0E1B\u0E1E\u0E25\u0E34\u0E40\u0E04\u0E0A\u0E31\u0E19\
  \u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13 \u0E0B\u0E36\u0E48\u0E07\u0E17\u0E33\u0E43\u0E2B\
  \u0E49\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E23\u0E31\u0E01\u0E29\u0E32\u0E02\u0E49\
  \u0E2D\u0E21\u0E39\u0E25, \u0E01\u0E32\u0E23\u0E01\u0E33\u0E2B\u0E19\u0E14\u0E04\
  \u0E48\u0E32, \u0E01\u0E32\u0E23\u0E1A\u0E31\u0E19\u0E17\u0E36\u0E01\u0E23\u0E32\
  \u0E22\u0E01\u0E32\u0E23,\u2026"
title: "\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E44\u0E1F\u0E25\u0E4C\u0E02\
  \u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21"
weight: 24
---

## อะไร & ทำไม?

การเขียนไฟล์ข้อความใน Clojure หมายถึงการสร้างหรือแก้ไขไฟล์เพื่อบันทึกข้อมูลนอกแอปพลิเคชันของคุณ ซึ่งทำให้สามารถรักษาข้อมูล, การกำหนดค่า, การบันทึกรายการ, หรือการสื่อสารระหว่างกระบวนการได้ โปรแกรมเมอร์ทำหน้าที่นี้เพื่อแยกสถานะแอปพลิเคชัน, การกำหนดค่า, หรือส่วนกลางข้อมูลระหว่างส่วนต่างๆ ของโปรแกรมหรือโปรแกรมต่างๆ กันออกไป.

## วิธีการ:

### เขียนข้อความไปยังไฟล์โดยใช้ฟังก์ชันที่มีอยู่ใน Clojure

ฟังก์ชัน `spit` คือวิธีที่ง่ายที่สุดในการเขียนข้อความลงในไฟล์ใน Clojure มันรับอาร์กิวเมนต์สองอย่าง: ที่อยู่ไฟล์และสตริงที่จะเขียน หากไฟล์นั้นไม่มีอยู่ `spit` จะสร้างมันขึ้นมา หากมีอยู่แล้ว `spit` จะเขียนทับ

```clojure
(spit "example.txt" "Hello, world!")
```

เพื่อเพิ่มข้อความไปยังไฟล์ที่มีอยู่แล้ว คุณสามารถใช้ฟังก์ชัน `spit` พร้อมกับตัวเลือก `:append`.

```clojure
(spit "example.txt" "\nLet's add this new line." :append true)
```

หลังจากรันส่วนประกอบเหล่านี้ "example.txt" จะมีข้อความ:

```
Hello, world!
Let's add this new line.
```

### การใช้ไลบรารีของบุคคลที่สาม

ตามที่ความสามารถภายในที่มากับ Clojure มักจะเพียงพอแล้ว ชุมชนได้พัฒนาไลบรารีที่ทรงพลังสำหรับงานที่ซับซ้อนหรือต้องการความเฉพาะเจาะจงมากขึ้น สำหรับไอ/โอไฟล์ หนึ่งในไลบรารียอดนิยมคือ `clojure.java.io` ซึ่งให้วิธีการจัดการไฟล์ที่คล้ายกับ Java มากขึ้น

ในการใช้ `clojure.java.io` สำหรับการเขียนไปยังไฟล์ คุณต้องนำเข้ามันก่อน:

```clojure
(require '[clojure.java.io :as io])
```

จากนั้น คุณสามารถใช้ฟังก์ชัน `writer` เพื่อรับอ็อบเจ็กต์ writer และฟังก์ชัน `spit` (หรือฟังก์ชันอื่นๆ เช่น `print`, `println`) เพื่อเขียนลงไปในไฟล์:

```clojure
(with-open [w (io/writer "example_with_io.txt")]
  (.write w "This is written using clojure.java.io"))
```

สิ่งนี้จะสร้าง (หรือเขียนทับหากมันมีอยู่แล้ว) "example_with_io.txt" ด้วยข้อความ:

```
This is written using clojure.java.io
```

จำไว้ว่า: `with-open` จะรับรองว่าไฟล์ถูกปิดอย่างถูกต้องหลังจากเขียน เพื่อหลีกเลี่ยงการรั่วไหลของทรัพยากร.
