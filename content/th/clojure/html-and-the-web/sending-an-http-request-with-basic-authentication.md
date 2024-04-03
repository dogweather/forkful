---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:51:17.256714-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E43\u0E19 Clojure, \u0E04\
  \u0E38\u0E13\u0E08\u0E30\u0E1B\u0E01\u0E15\u0E34\u0E43\u0E0A\u0E49\u0E44\u0E25\u0E1A\
  \u0E23\u0E32\u0E23\u0E35 `clj-http` \u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E04\u0E33\
  \u0E02\u0E2D HTTP, \u0E23\u0E27\u0E21\u0E16\u0E36\u0E07\u0E04\u0E33\u0E02\u0E2D\u0E14\
  \u0E49\u0E27\u0E22\u0E01\u0E32\u0E23\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E2A\
  \u0E34\u0E17\u0E18\u0E34\u0E4C\u0E1E\u0E37\u0E49\u0E19\u0E10\u0E32\u0E19 \u0E21\u0E32\
  \u0E40\u0E23\u0E34\u0E48\u0E21\u0E01\u0E31\u0E19\u0E14\u0E49\u0E27\u0E22\u0E01\u0E32\
  \u0E23\u0E40\u0E1E\u0E34\u0E48\u0E21 dependency (`[clj-http\u2026"
lastmod: '2024-03-17T21:57:55.806721-06:00'
model: gpt-4-0125-preview
summary: "\u0E43\u0E19 Clojure, \u0E04\u0E38\u0E13\u0E08\u0E30\u0E1B\u0E01\u0E15\u0E34\
  \u0E43\u0E0A\u0E49\u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\u0E35 `clj-http` \u0E2A\u0E33\
  \u0E2B\u0E23\u0E31\u0E1A\u0E04\u0E33\u0E02\u0E2D HTTP, \u0E23\u0E27\u0E21\u0E16\u0E36\
  \u0E07\u0E04\u0E33\u0E02\u0E2D\u0E14\u0E49\u0E27\u0E22\u0E01\u0E32\u0E23\u0E15\u0E23\
  \u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E2A\u0E34\u0E17\u0E18\u0E34\u0E4C\u0E1E\u0E37\u0E49\
  \u0E19\u0E10\u0E32\u0E19 \u0E21\u0E32\u0E40\u0E23\u0E34\u0E48\u0E21\u0E01\u0E31\u0E19\
  \u0E14\u0E49\u0E27\u0E22\u0E01\u0E32\u0E23\u0E40\u0E1E\u0E34\u0E48\u0E21 dependency\
  \ (`[clj-http \"3.12.3\"]` \u0E15\u0E32\u0E21\u0E01\u0E32\u0E23\u0E1B\u0E23\u0E31\
  \u0E1A\u0E1B\u0E23\u0E38\u0E07\u0E04\u0E23\u0E31\u0E49\u0E07\u0E25\u0E48\u0E32\u0E2A\
  \u0E38\u0E14\u0E02\u0E2D\u0E07\u0E09\u0E31\u0E19) \u0E44\u0E1B\u0E22\u0E31\u0E07\
  \ `project.clj` \u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13\n\n\u0E15\u0E48\u0E2D\u0E44\
  \u0E1B, \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E27\u0E34\u0E18\u0E35\u0E17\u0E35\
  \u0E48\u0E04\u0E38\u0E13\u0E08\u0E31\u0E14\u0E1C\u0E25\u0E34\u0E15\u0E04\u0E33\u0E02\
  \u0E2D GET \u0E01\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\
  \u0E1A\u0E2A\u0E34\u0E17\u0E18\u0E34\u0E4C\u0E1E\u0E37\u0E49\u0E19\u0E10\u0E32\u0E19\
  ."
title: "\u0E01\u0E32\u0E23\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP \u0E14\u0E49\
  \u0E27\u0E22\u0E01\u0E32\u0E23\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E2A\u0E34\
  \u0E17\u0E18\u0E34\u0E4C\u0E1E\u0E37\u0E49\u0E19\u0E10\u0E32\u0E19"
weight: 45
---

## วิธีการ:
ใน Clojure, คุณจะปกติใช้ไลบรารี `clj-http` สำหรับคำขอ HTTP, รวมถึงคำขอด้วยการตรวจสอบสิทธิ์พื้นฐาน มาเริ่มกันด้วยการเพิ่ม dependency (`[clj-http "3.12.3"]` ตามการปรับปรุงครั้งล่าสุดของฉัน) ไปยัง `project.clj` ของคุณ

ต่อไป, นี่คือวิธีที่คุณจัดผลิตคำขอ GET กับการตรวจสอบสิทธิ์พื้นฐาน:

```clojure
(require '[clj-http.client :as client])

(let [response (client/get "https://your-api.com/resource"
                           {:basic-auth ["username" "password"]})]
  (println "สถานะ:" (:status response))
  (println "ข้อมูล:" (:body response)))
```
แทนที่ `"https://your-api.com/resource"`, `"username"`, และ `"password"` ด้วยรายละเอียดของคุณ โค้ดนี้ส่งคำขอ GET และพิมพ์สถานะและข้อมูลตอบกลับ

ตัวอย่างผลลัพธ์อาจดูเช่นนี้:

```
สถานะ: 200
ข้อมูล: {ข้อมูล JSON หรืออื่น ๆ ที่นี่}
```

## ข้อมูลลึก
HTTP การตรวจสอบสิทธิ์พื้นฐานมีรากฐานมาจากโปรโตคอลเว็บในช่วงต้น มันส่งชื่อผู้ใช้และรหัสผ่านในหัวข้อ HTTP ด้วยการเข้ารหัส Base64 แม้ว่าจะง่าย แต่ก็ไม่ใช่วิธีที่ปลอดภัยที่สุดเพราะข้อมูลรับรองสามารถถอดรหัสได้ง่ายหากถูกดักฟัง

ทางเลือก:
- **การตรวจสอบสิทธิ์ Digest**: ซับซ้อนกว่า, เกี่ยวข้องกับการส่งรหัสผ่านที่ถูกแฮชแทน
- **OAuth**: ระบบการอนุญาตที่เข้มแข็งกว่าซึ่งไม่ต้องการการส่งชื่อผู้ใช้และรหัสผ่าน
- **กุญแจ API**: โทเค็นที่เป็นเอกลักษณ์ที่ใช้แทนข้อมูลรับรองการล็อกอินแบบดั้งเดิม

ในเบื้องหลังใน `clj-http`, การระบุ `:basic-auth` ใน hashmap ตัวเลือกทำให้ไลบรารีเข้ารหัสข้อมูลรับรองของคุณและติดตามพวกมันไปที่หัวข้อ HTTP `Authorization` เมื่อเซิร์ฟเวอร์ได้รับคำขอ, มันจะถอดรหัสหัวข้อและตรวจสอบข้อมูลรับรอง

จำไว้ว่าสำหรับการส่งข้อมูลที่ปลอดภัย, ควรใช้ HTTPS เพื่อป้องกันไม่ให้คนอื่นดักฟังข้อมูลรับรองของคุณ

## ดูเพิ่มเติม
- ที่เก็บของ clj-http GitHub: https://github.com/dakrone/clj-http
- เอกสารอย่างเป็นทางการของ Clojure: https://clojure.org/
- การตรวจสอบสิทธิ์ HTTP ใน MDN: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication 
- ความเข้าใจ OAuth: https://oauth.net/
