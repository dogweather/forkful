---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:10.642734-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: Clojure \u0E44\u0E21\u0E48\
  \u0E21\u0E35\u0E01\u0E32\u0E23\u0E2A\u0E19\u0E31\u0E1A\u0E2A\u0E19\u0E38\u0E19 YAML\
  \ \u0E40\u0E1B\u0E47\u0E19 built-in, \u0E41\u0E15\u0E48\u0E04\u0E38\u0E13\u0E2A\u0E32\
  \u0E21\u0E32\u0E23\u0E16\u0E43\u0E0A\u0E49\u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\u0E35\
  \u0E02\u0E2D\u0E07\u0E1A\u0E38\u0E04\u0E04\u0E25\u0E17\u0E35\u0E48\u0E2A\u0E32\u0E21\
  \ \u0E40\u0E0A\u0E48\u0E19 `clj-yaml` \u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\
  \u0E32\u0E23\u0E41\u0E22\u0E01\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\u0E4C\
  \u0E41\u0E25\u0E30\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\
  \ YAML\u2026"
lastmod: '2024-03-17T21:57:55.828967-06:00'
model: gpt-4-0125-preview
summary: "Clojure \u0E44\u0E21\u0E48\u0E21\u0E35\u0E01\u0E32\u0E23\u0E2A\u0E19\u0E31\
  \u0E1A\u0E2A\u0E19\u0E38\u0E19 YAML \u0E40\u0E1B\u0E47\u0E19 built-in, \u0E41\u0E15\
  \u0E48\u0E04\u0E38\u0E13\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E43\u0E0A\u0E49\u0E44\
  \u0E25\u0E1A\u0E23\u0E32\u0E23\u0E35\u0E02\u0E2D\u0E07\u0E1A\u0E38\u0E04\u0E04\u0E25\
  \u0E17\u0E35\u0E48\u0E2A\u0E32\u0E21 \u0E40\u0E0A\u0E48\u0E19 `clj-yaml` \u0E2A\u0E33\
  \u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E27\u0E34\u0E40\u0E04\
  \u0E23\u0E32\u0E30\u0E2B\u0E4C\u0E41\u0E25\u0E30\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E02\
  \u0E49\u0E2D\u0E21\u0E39\u0E25 YAML \u0E01\u0E48\u0E2D\u0E19\u0E2D\u0E37\u0E48\u0E19\
  , \u0E40\u0E1E\u0E34\u0E48\u0E21\u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\u0E35\u0E40\
  \u0E02\u0E49\u0E32\u0E01\u0E31\u0E1A dependencies \u0E02\u0E2D\u0E07\u0E42\u0E1B\
  \u0E23\u0E40\u0E08\u0E01\u0E15\u0E4C\u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13."
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A YAML"
weight: 41
---

## วิธีการ:
Clojure ไม่มีการสนับสนุน YAML เป็น built-in, แต่คุณสามารถใช้ไลบรารีของบุคคลที่สาม เช่น `clj-yaml` สำหรับการแยกวิเคราะห์และสร้างข้อมูล YAML ก่อนอื่น, เพิ่มไลบรารีเข้ากับ dependencies ของโปรเจกต์ของคุณ:

```clojure
;; เพิ่มสิ่งนี้ลงใน dependencies ของ project.clj ของคุณ
[clj-yaml "0.7.0"]
```

นี่คือวิธีที่คุณสามารถใช้ `clj-yaml` ในการแยกวิเคราะห์ YAML และแปลงแผนที่ Clojure เป็น YAML

### การแยกวิเคราะห์ YAML:
```clojure
(require '[clj-yaml.core :as yaml])

;; การแยกวิเคราะห์สตริง YAML
(let [yaml-str "name: John Doe\nage: 30\nlanguages:\n  - Clojure\n  - Python"]
  (yaml/parse-string yaml-str))
;; ผลลัพธ์:
;; => {"name" "John Doe", "age" 30, "languages" ["Clojure" "Python"]}
```

### การสร้าง YAML จาก Clojure:
```clojure
(require '[clj-yaml.core :as yaml])

;; การแปลงแผนที่ Clojure เป็นสตริง YAML
(let [data-map {:name "Jane Doe" :age 28 :languages ["Java" "Ruby"]}]
  (yaml/generate-string data-map))
;; ผลลัพธ์:
; "age: 28\nlanguages:\n- Java\n- Ruby\nname: Jane Doe\n"
```

การดำเนินการง่ายๆ เหล่านี้กับ `clj-yaml` สามารถรวมเข้ากับการใช้งานของ Clojure เพื่อจัดการกับไฟล์การตั้งค่าหรือส่งข้อมูลระหว่างบริการหรือส่วนประกอบอื่นๆ ที่ใช้ YAML.
