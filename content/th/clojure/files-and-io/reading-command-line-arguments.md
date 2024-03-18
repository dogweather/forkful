---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:49:35.416881-06:00
description: "\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E2D\u0E32\u0E23\u0E4C\u0E01\
  \u0E34\u0E27\u0E40\u0E21\u0E19\u0E15\u0E4C\u0E1A\u0E19 command line \u0E0A\u0E48\
  \u0E27\u0E22\u0E43\u0E2B\u0E49\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E2A\u0E32\
  \u0E21\u0E32\u0E23\u0E16\u0E23\u0E31\u0E1A\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E42\
  \u0E14\u0E22\u0E15\u0E23\u0E07\u0E08\u0E32\u0E01\u0E04\u0E33\u0E2A\u0E31\u0E48\u0E07\
  \u0E1A\u0E19\u0E40\u0E17\u0E2D\u0E23\u0E4C\u0E21\u0E34\u0E19\u0E2D\u0E25\u0E02\u0E2D\
  \u0E07\u0E1C\u0E39\u0E49\u0E43\u0E0A\u0E49\u2026"
lastmod: '2024-03-17T21:57:55.823982-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E2D\u0E32\u0E23\u0E4C\u0E01\
  \u0E34\u0E27\u0E40\u0E21\u0E19\u0E15\u0E4C\u0E1A\u0E19 command line \u0E0A\u0E48\
  \u0E27\u0E22\u0E43\u0E2B\u0E49\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E2A\u0E32\
  \u0E21\u0E32\u0E23\u0E16\u0E23\u0E31\u0E1A\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E42\
  \u0E14\u0E22\u0E15\u0E23\u0E07\u0E08\u0E32\u0E01\u0E04\u0E33\u0E2A\u0E31\u0E48\u0E07\
  \u0E1A\u0E19\u0E40\u0E17\u0E2D\u0E23\u0E4C\u0E21\u0E34\u0E19\u0E2D\u0E25\u0E02\u0E2D\
  \u0E07\u0E1C\u0E39\u0E49\u0E43\u0E0A\u0E49\u2026"
title: "\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E2D\u0E32\u0E23\u0E4C\u0E01\u0E34\
  \u0E27\u0E40\u0E21\u0E19\u0E15\u0E4C\u0E08\u0E32\u0E01\u0E04\u0E33\u0E2A\u0E31\u0E48\
  \u0E07\u0E25\u0E33\u0E14\u0E31\u0E1A"
---

{{< edit_this_page >}}

## อะไร & ทำไม?

การอ่านอาร์กิวเมนต์บน command line ช่วยให้โปรแกรมสามารถรับข้อมูลโดยตรงจากคำสั่งบนเทอร์มินอลของผู้ใช้ โปรแกรมเมอร์ทำเช่นนี้เพื่อปรับแต่งพฤติกรรมของโปรแกรมโดยไม่ต้องเปลี่ยนแปลงโค้ดเอง

## วิธีทำ:

ใน Clojure, คุณสามารถดึงอาร์กิวเมนต์บน command line ได้โดยใช้ `*command-line-args*` นี่คือตัวอย่างง่ายๆ:

```clojure
;; สมมติโค้ดนี้อยู่ในไฟล์ที่ชื่อว่า `echo.clj`

(defn -main [& args]
  (println "คุณได้ใส่:" args))

;; เพื่อรัน: `clojure echo.clj arg1 arg2 arg3`
```

ผลลัพธ์ตัวอย่าง:

```
คุณได้ใส่: (arg1 arg2 arg3)
```

ต้องการประมวลผลหรือไม่? ใช้ฟังก์ชั่นการรวบรวมของ Clojure

```clojure
(defn -main [& args]
  (let [processed-args (mapv str/upper-case args)]
    (println "เปลี่ยนเป็นตัวพิมพ์ใหญ่:" processed-args)))

;; ตอนนี้, การรัน `clojure echo.clj hello world` จะแสดงผล:
```

ผลลัพธ์ตัวอย่าง:

```
เปลี่ยนเป็นตัวพิมพ์ใหญ่: ["HELLO" "WORLD"]
```

## ลงลึก

`*command-line-args*` เป็น var ใน Clojure, ถูกตั้งค่าเป็นลำดับของอาร์กิวเมนต์ที่ส่งไปยังสคริปต์ มันเป็นอย่างนี้มาตั้งแต่ต้นยุคของ Clojure, แสดงให้เห็นว่า Clojure มองอาร์กิวเมนต์บน command line เป็นสิ่งสำคัญ

มีทางเลือกอื่นหรือ? กลไกของ Java สำหรับการดึงอาร์กิวเมนต์บน command line ก็ทำงานใน Clojure ได้เช่นกัน, ขอบคุณความสามารถในการทำงานร่วมกัน แต่นั่นยากกว่าในการใช้งาน

สำหรับรายละเอียดในการทำงาน, เมื่อ Clojure เริ่มต้น, มันจะวิเคราะห์อาร์กิวเมนต์และเก็บไว้ใน `*command-line-args*` สคริปต์ของคุณจากนั้นสามารถทำอะไรกับมันก็ได้—วิเคราะห์, ทอดทิ้ง, แปลง, คุณตั้งชื่อมัน

## ดูเพิ่มเติม

- เครื่องมือ CLI อย่างเป็นทางการของ Clojure: https://clojure.org/guides/deps_and_cli
- Clojure จากพื้นฐานไปถึงการเขียนสคริปต์บน command line: https://aphyr.com/posts/305-clojure-from-the-ground-up-command-line
- ClojureDocs บน *command-line-args*: https://clojuredocs.org/clojure.core/*command-line-args*
