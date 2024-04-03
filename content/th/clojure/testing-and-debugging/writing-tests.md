---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:53.207057-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: Clojure, \u0E42\u0E14\u0E22\
  \u0E43\u0E0A\u0E49\u0E1B\u0E23\u0E30\u0E42\u0E22\u0E0A\u0E19\u0E4C\u0E08\u0E32\u0E01\
  \ JVM, \u0E23\u0E2D\u0E07\u0E23\u0E31\u0E1A\u0E40\u0E1F\u0E23\u0E21\u0E40\u0E27\u0E34\
  \u0E23\u0E4C\u0E04\u0E01\u0E32\u0E23\u0E17\u0E14\u0E2A\u0E2D\u0E1A\u0E2B\u0E25\u0E32\
  \u0E22\u0E2D\u0E22\u0E48\u0E32\u0E07 \u0E2D\u0E22\u0E48\u0E32\u0E07\u0E44\u0E23\u0E01\
  \u0E47\u0E15\u0E32\u0E21, \u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\u0E35\u0E17\u0E35\
  \u0E48\u0E21\u0E35\u0E2D\u0E22\u0E39\u0E48\u0E20\u0E32\u0E22\u0E43\u0E19\u0E17\u0E35\
  \u0E48\u0E43\u0E0A\u0E49\u0E01\u0E31\u0E19\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E41\u0E1E\
  \u0E23\u0E48\u0E2B\u0E25\u0E32\u0E22\u0E04\u0E37\u0E2D `clojure.test`\u2026"
lastmod: '2024-03-17T21:57:55.811644-06:00'
model: gpt-4-0125-preview
summary: "Clojure, \u0E42\u0E14\u0E22\u0E43\u0E0A\u0E49\u0E1B\u0E23\u0E30\u0E42\u0E22\
  \u0E0A\u0E19\u0E4C\u0E08\u0E32\u0E01 JVM, \u0E23\u0E2D\u0E07\u0E23\u0E31\u0E1A\u0E40\
  \u0E1F\u0E23\u0E21\u0E40\u0E27\u0E34\u0E23\u0E4C\u0E04\u0E01\u0E32\u0E23\u0E17\u0E14\
  \u0E2A\u0E2D\u0E1A\u0E2B\u0E25\u0E32\u0E22\u0E2D\u0E22\u0E48\u0E32\u0E07 \u0E2D\u0E22\
  \u0E48\u0E32\u0E07\u0E44\u0E23\u0E01\u0E47\u0E15\u0E32\u0E21, \u0E44\u0E25\u0E1A\
  \u0E23\u0E32\u0E23\u0E35\u0E17\u0E35\u0E48\u0E21\u0E35\u0E2D\u0E22\u0E39\u0E48\u0E20\
  \u0E32\u0E22\u0E43\u0E19\u0E17\u0E35\u0E48\u0E43\u0E0A\u0E49\u0E01\u0E31\u0E19\u0E2D\
  \u0E22\u0E48\u0E32\u0E07\u0E41\u0E1E\u0E23\u0E48\u0E2B\u0E25\u0E32\u0E22\u0E04\u0E37\
  \u0E2D `clojure.test` \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E15\u0E31\u0E27\u0E2D\
  \u0E22\u0E48\u0E32\u0E07\u0E07\u0E48\u0E32\u0E22\u0E46."
title: "\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E01\u0E32\u0E23\u0E17\u0E14\
  \u0E2A\u0E2D\u0E1A"
weight: 36
---

## วิธีการ:
Clojure, โดยใช้ประโยชน์จาก JVM, รองรับเฟรมเวิร์คการทดสอบหลายอย่าง อย่างไรก็ตาม, ไลบรารีที่มีอยู่ภายในที่ใช้กันอย่างแพร่หลายคือ `clojure.test` นี่คือตัวอย่างง่ายๆ:

```clojure
(ns example.test
  (:require [clojure.test :refer :all]
            [example.core :refer :all]))

(deftest test-addition
  (testing "ความสามารถในการบวก"
    (is (= 4 (add 2 2)))
    (is (= 7 (add 3 4)))))

(run-tests)
```
หลังจากที่รันเทสนี้, คุณจะเห็นผลลัพธ์ที่คล้ายกับ:

```
Testing example.test

รัน 2 เทสที่มี 2 การยืนยัน
0 ความล้มเหลว, 0 ข้อผิดพลาด
```

สำหรับผู้ที่ต้องการตัวเลือกที่มีความหลากหลายมากขึ้น, สามารถใช้ไลบรารีของบุคคลที่สาม เช่น `Midje` หรือ `test.check` นี่คือวิธีที่คุณอาจจะใช้ Midje สำหรับเทสที่คล้ายกัน:

ประการแรก, เพิ่ม Midje ในการพึ่งพา project.clj ของคุณ:
```clojure
[midje "1.9.9"]
```

จากนั้น, เทสของคุณด้วย Midje อาจจะดูเหมือนนี้:

```clojure
(ns example.test
  (:require [midje.sweet :refer :all]
            [example.core :refer :all]))

(fact "การทดสอบการบวก"
  (add 2 2) => 4
  (add 3 4) => 7)
```

เมื่อรันเทสผ่าน Midje ด้วย `lein midje`, ผลลัพธ์จะแสดงสิ่งที่คล้ายกับ:

```
การตรวจสอบทั้งหมด (2) สำเร็จ
```
