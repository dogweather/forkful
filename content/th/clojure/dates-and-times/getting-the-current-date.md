---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:47:08.710346-06:00
description: "\u0E01\u0E32\u0E23\u0E23\u0E31\u0E1A\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\
  \u0E1B\u0E31\u0E08\u0E08\u0E38\u0E1A\u0E31\u0E19\u0E43\u0E19\u0E01\u0E32\u0E23\u0E40\
  \u0E02\u0E35\u0E22\u0E19\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E21\u0E35\u0E04\
  \u0E27\u0E32\u0E21\u0E2A\u0E33\u0E04\u0E31\u0E0D\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E21\
  \u0E32\u0E01\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E40\u0E2B\u0E15\u0E38\u0E1C\u0E25\
  \u0E21\u0E32\u0E01\u0E21\u0E32\u0E22 \u0E23\u0E27\u0E21\u0E16\u0E36\u0E07\u0E01\u0E32\
  \u0E23\u0E1A\u0E31\u0E19\u0E17\u0E36\u0E01, \u0E01\u0E32\u0E23\u0E1B\u0E23\u0E30\
  \u0E17\u0E31\u0E1A\u0E40\u0E27\u0E25\u0E32\u0E40\u0E2B\u0E15\u0E38\u0E01\u0E32\u0E23\
  \u0E13\u0E4C, \u0E41\u0E25\u0E30\u0E01\u0E32\u0E23\u0E01\u0E33\u0E2B\u0E19\u0E14\
  \u0E01\u0E32\u0E23\u0E07\u0E32\u0E19 \u0E43\u0E19 Clojure, \u0E20\u0E32\u0E29\u0E32\
  \u2026"
lastmod: '2024-03-17T21:57:55.818797-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E23\u0E31\u0E1A\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\
  \u0E1B\u0E31\u0E08\u0E08\u0E38\u0E1A\u0E31\u0E19\u0E43\u0E19\u0E01\u0E32\u0E23\u0E40\
  \u0E02\u0E35\u0E22\u0E19\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E21\u0E35\u0E04\
  \u0E27\u0E32\u0E21\u0E2A\u0E33\u0E04\u0E31\u0E0D\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E21\
  \u0E32\u0E01\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E40\u0E2B\u0E15\u0E38\u0E1C\u0E25\
  \u0E21\u0E32\u0E01\u0E21\u0E32\u0E22 \u0E23\u0E27\u0E21\u0E16\u0E36\u0E07\u0E01\u0E32\
  \u0E23\u0E1A\u0E31\u0E19\u0E17\u0E36\u0E01, \u0E01\u0E32\u0E23\u0E1B\u0E23\u0E30\
  \u0E17\u0E31\u0E1A\u0E40\u0E27\u0E25\u0E32\u0E40\u0E2B\u0E15\u0E38\u0E01\u0E32\u0E23\
  \u0E13\u0E4C, \u0E41\u0E25\u0E30\u0E01\u0E32\u0E23\u0E01\u0E33\u0E2B\u0E19\u0E14\
  \u0E01\u0E32\u0E23\u0E07\u0E32\u0E19 \u0E43\u0E19 Clojure, \u0E20\u0E32\u0E29\u0E32\
  \u2026"
title: "\u0E01\u0E32\u0E23\u0E23\u0E31\u0E1A\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E1B\
  \u0E31\u0E08\u0E08\u0E38\u0E1A\u0E31\u0E19"
---

{{< edit_this_page >}}

## อะไรและทำไม?
การรับวันที่ปัจจุบันในการเขียนโปรแกรมมีความสำคัญอย่างมากสำหรับเหตุผลมากมาย รวมถึงการบันทึก, การประทับเวลาเหตุการณ์, และการกำหนดการงาน ใน Clojure, ภาษา Lisp บน JVM, งานนี้ใช้ความสามารถของ Java interop, ช่วยให้สามารถเข้าถึง Java Date-Time API ที่ร่ำรวยได้อย่างง่ายดาย

## วิธีการ:

### การใช้ Java Interop
ความสามารถในการทำงานร่วมกับ Java อย่างไม่มีรอยต่อของ Clojure ช่วยให้คุณสามารถเข้าถึง Java Date-Time API ได้โดยตรง นี่คือวิธีที่คุณสามารถรับวันที่ปัจจุบัน:

```clojure
(import java.time.LocalDate)

(defn get-current-date []
  (str (LocalDate/now)))

;; ตัวอย่างผลลัพธ์
(get-current-date) ; "2023-04-15"
```

### การใช้ไลบรารี clj-time
สำหรับโซลูชัน Clojure ที่ตรงกับวิธีการเขียนโค้ดมากขึ้น คุณอาจเลือกใช้ไลบรารี `clj-time` ซึ่งเป็นตัวห่อรอบ Joda-Time แต่สำหรับโปรเจกต์ใหม่ๆ โดยมากแนะนำให้ใช้ Java 8 Date-Time API ที่มาในตัว อย่างไรก็ตาม หากคุณต้องการหรือจำเป็นต้องใช้ `clj-time`:

สิ่งแรก, เพิ่ม `clj-time` ลงในการพึ่งพาโปรเจกต์ของคุณ ในไฟล์ `project.clj` ของคุณ ให้รวม:

```clojure
[clj-time "0.15.2"]
```

จากนั้น, ใช้มันเพื่อรับวันที่ปัจจุบัน:

```clojure
(require '[clj-time.core :as time])

(defn get-current-date-clj-time []
  (str (time/now)))

;; ตัวอย่างผลลัพธ์
(get-current-date-clj-time) ; "2023-04-15T12:34:56.789Z"
```

ทั้งสองวิธีนี้ให้วิธีการที่รวดเร็วและมีประสิทธิภาพในการรับวันที่ปัจจุบันใน Clojure โดยใช้พลังของแพลตฟอร์ม Java ที่อยู่เบื้องหลังหรือความสะดวกของไลบรารีที่เฉพาะเจาะจงกับ Clojure
