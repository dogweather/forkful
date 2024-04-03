---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:13.635342-06:00
description: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A TOML\
  \ \u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\
  \u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\u0E4C (parsing) \u0E41\u0E25\u0E30\
  \u0E2A\u0E23\u0E49\u0E32\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25 TOML (Tom's Obvious,\
  \ Minimal Language) \u0E14\u0E49\u0E27\u0E22 Haskell.\u2026"
lastmod: '2024-03-17T21:57:56.291175-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A TOML\
  \ \u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\
  \u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\u0E4C (parsing) \u0E41\u0E25\u0E30\
  \u0E2A\u0E23\u0E49\u0E32\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25 TOML (Tom's Obvious,\
  \ Minimal Language) \u0E14\u0E49\u0E27\u0E22 Haskell."
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E23\u0E48\u0E27\u0E21\u0E01\
  \u0E31\u0E1A TOML"
weight: 39
---

## อะไร & ทำไม?
การทำงานกับ TOML หมายถึงการแยกวิเคราะห์ (parsing) และสร้างข้อมูล TOML (Tom's Obvious, Minimal Language) ด้วย Haskell. โปรแกรมเมอร์ทำสิ่งนี้เพื่อจัดการไฟล์การตั้งค่าหรือแลกเปลี่ยนข้อมูลได้อย่างง่ายดายด้วยการรับประกันประเภทข้อมูลที่แข็งแกร่งและไวยากรณ์ที่เรียบง่าย.

## วิธีการ:
ขั้นแรก, ตรวจสอบว่าคุณมีไลบรารีสำหรับแยกวิเคราะห์ TOML หรือไม่ สำหรับ Haskell, `htoml` เป็นตัวเลือกยอดนิยม คุณจะต้องเพิ่มเข้าไปในการพึ่งพาโปรเจคของคุณ

```Haskell
-- นำเข้าไลบรารีสำหรับแยกวิเคราะห์ TOML
import qualified Text.Toml as Toml

-- กำหนดโครงสร้างข้อมูลการตั้งค่า
data Config = Config {
  title :: String,
  owner :: Owner
} deriving (Show)

data Owner = Owner {
  name :: String,
  dob :: Maybe Day -- วันที่ที่เป็นตัวเลือก
} deriving (Show)

-- การแยกวิเคราะห์สตริง TOML
main :: IO ()
main = do
  let tomlData = "[owner]\nname = \"Tom Preston-Werner\"\ndob = 1979-05-27T07:32:00Z"
  case Toml.parseTomlDoc "" tomlData of
    Left err -> putStrLn $ "Error: " ++ show err
    Right toml -> print toml -- หรือกระบวนการวิเคราะห์ TOML เพิ่มเติม
```

เอาต์พุตตัวอย่างสามารถโครงสร้างและเข้าถึงได้เหมือนกับประเภทข้อมูล Haskell อื่นๆ

## ดำดิ่งลึก
โดยประวัติศาสตร์, TOML ถูกสร้างโดย Tom Preston-Werner, ผู้ร่วมก่อตั้ง GitHub, เป็นการตอบสนองต่อความซับซ้อนของ YAML และ JSON สำหรับไฟล์การตั้งค่า มันเน้นไว้ว่าการอ่านและเขียนง่ายกว่า JSON, และเข้มงวดและเรียบง่ายกว่า YAML.

ทางเลือกอื่นต่อ TOML ประกอบด้วย JSON และ YAML, โดยแต่ละรูปแบบมีจุดแข็งของตัวเอง JSON เป็นที่แพร่หลายและไม่เกี่ยวข้องกับภาษา, ในขณะที่ YAML มีรูปแบบที่อ่านได้ง่ายกว่าสำหรับมนุษย์ TOML ได้รับค่านิยมสำหรับความเรียบง่ายและความสอดคล้อง, หลีกเลี่ยงบางปัญหาของญาติของมัน

การดำเนินการใน Haskell โดยทั่วไปเกี่ยวข้องกับการใช้ไลบรารีที่แยกวิเคราะห์ TOML เป็นประเภทข้อมูล Haskell, มักใช้ระบบประเภทขั้นสูงของ Haskell เพื่อรับประกันความถูกต้อง การแยกวิเคราะห์สามารถทำได้ผ่านการเดินลงไปแบบเวียนกลับหรือการแยกวิเคราะห์แบบคอมบิเนเตอร์, ซึ่งเป็นการทรงตัวระหว่างประสิทธิภาพกับความสามารถในการอ่านและการรักษาโค้ด

## ดูเพิ่มเติม
- `htoml`: https://hackage.haskell.org/package/htoml
- ที่เก็บ GitHub อย่างเป็นทางการของ TOML: https://github.com/toml-lang/toml
- การเปรียบเทียบรูปแบบการซีเรียลไลซ์ข้อมูล: https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats
