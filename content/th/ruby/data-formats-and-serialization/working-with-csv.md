---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:52:39.254488-06:00
description: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A\u0E44\
  \u0E1F\u0E25\u0E4C CSV \u0E43\u0E19 Ruby \u0E43\u0E2B\u0E49\u0E27\u0E34\u0E18\u0E35\
  \u0E01\u0E32\u0E23\u0E17\u0E35\u0E48\u0E15\u0E23\u0E07\u0E44\u0E1B\u0E15\u0E23\u0E07\
  \u0E21\u0E32\u0E43\u0E19\u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E02\
  \u0E49\u0E2D\u0E21\u0E39\u0E25\u0E41\u0E1A\u0E1A\u0E15\u0E32\u0E23\u0E32\u0E07 \u0E42\
  \u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E21\u0E31\u0E01\
  \u0E43\u0E0A\u0E49\u0E27\u0E34\u0E18\u0E35\u0E19\u0E35\u0E49\u0E2A\u0E33\u0E2B\u0E23\
  \u0E31\u0E1A\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\
  , \u0E01\u0E32\u0E23\u0E2A\u0E01\u0E31\u0E14\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\
  , \u0E01\u0E32\u0E23\u0E40\u0E1B\u0E25\u0E35\u0E48\u0E22\u0E19\u0E41\u0E1B\u0E25\
  \u0E07,\u2026"
lastmod: '2024-03-17T21:57:56.750568-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A\u0E44\
  \u0E1F\u0E25\u0E4C CSV \u0E43\u0E19 Ruby \u0E43\u0E2B\u0E49\u0E27\u0E34\u0E18\u0E35\
  \u0E01\u0E32\u0E23\u0E17\u0E35\u0E48\u0E15\u0E23\u0E07\u0E44\u0E1B\u0E15\u0E23\u0E07\
  \u0E21\u0E32\u0E43\u0E19\u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E02\
  \u0E49\u0E2D\u0E21\u0E39\u0E25\u0E41\u0E1A\u0E1A\u0E15\u0E32\u0E23\u0E32\u0E07 \u0E42\
  \u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E21\u0E31\u0E01\
  \u0E43\u0E0A\u0E49\u0E27\u0E34\u0E18\u0E35\u0E19\u0E35\u0E49\u0E2A\u0E33\u0E2B\u0E23\
  \u0E31\u0E1A\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\
  , \u0E01\u0E32\u0E23\u0E2A\u0E01\u0E31\u0E14\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\
  , \u0E01\u0E32\u0E23\u0E40\u0E1B\u0E25\u0E35\u0E48\u0E22\u0E19\u0E41\u0E1B\u0E25\
  \u0E07,\u2026"
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A CSV"
---

{{< edit_this_page >}}

## อะไรและทำไม?

การทำงานกับไฟล์ CSV ใน Ruby ให้วิธีการที่ตรงไปตรงมาในการจัดการข้อมูลแบบตาราง โปรแกรมเมอร์มักใช้วิธีนี้สำหรับการแยกข้อมูล, การสกัดข้อมูล, การเปลี่ยนแปลง, และการจัดเก็บทำให้เป็นทักษะสำคัญสำหรับงานที่เกี่ยวข้องกับการจัดการหรือการวิเคราะห์ข้อมูล

## วิธีทำ:

Ruby มีไลบรารี CSV มาให้เป็นค่าตั้งต้น ซึ่งทำให้การอ่านจากและเขียนไปยังไฟล์ CSV ง่ายขึ้น นี่คือวิธีที่คุณสามารถใช้ประโยชน์จากสิ่งนี้สำหรับงานทั่วไป:

### การอ่านไฟล์ CSV
เพื่ออ่านจากไฟล์ CSV, คุณต้องมีไลบรารี CSV ก่อน จากนั้นคุณสามารถวนลูปท่องไปตามแถว หรืออ่านไปยัง array

```ruby
require 'csv'

# การอ่านแต่ละแถวเป็น array
CSV.foreach("data.csv") do |row|
  puts row.inspect
end

# ผลลัพธ์สำหรับแต่ละแถวอาจจะแสดงเป็น: ["data1", "data2", "data3"]
```

### เขียนไปยัง CSV 
การเขียนไปยังไฟล์ CSV ก็ง่ายเช่นกัน คุณสามารถเพิ่มข้อมูลเข้าไปในไฟล์ที่มีอยู่แล้วหรือสร้างไฟล์ใหม่เพื่อเขียน

```ruby
require 'csv'

CSV.open("output.csv", "wb") do |csv|
  csv << ["header1", "header2", "header3"]
  csv << ["value1", "value2", "value3"]
end

# สิ่งนี้สร้างหรือเขียนทับ 'output.csv' ด้วยหัวข้อและค่าที่ระบุ
```

### การแยกสตริง CSV
บางครั้งคุณต้องการแยกข้อมูล CSV โดยตรงจากสตริง นี่คือวิธีการ:

```ruby
require 'csv'

data = "name,age,city\nJohn Doe,29,New York\nJane Doe,31,Chicago"
csv = CSV.parse(data, headers: true)

csv.each do |row|
  puts "#{row['name']} - #{row['age']} - #{row['city']}"
end

# ผลลัพธ์ที่คาดหวัง:
# John Doe - 29 - New York
# Jane Doe - 31 - Chicago
```

### การใช้ SmarterCSV
สำหรับงาน CSV ที่ซับซ้อนมากขึ้น, `SmarterCSV` gem สามารถเป็นเครื่องมือที่มีค่า ก่อนอื่น, ติดตั้ง gem:

```shell
gem install smarter_csv
```

จากนั้นคุณสามารถใช้มันในการจัดการไฟล์ขนาดใหญ่หรือทำการแยกและจัดการที่ซับซ้อนมากขึ้น:

```ruby
require 'smarter_csv'

options = {}
data = SmarterCSV.process('large_data.csv', options)

data.each do |hash|
  puts hash.inspect
end

# สิ่งนี้จะอ่าน 'large_data.csv' และแสดงผลแต่ละแถวเป็น hash โดยอิงจากหัวข้อ
```

สรุป, ไลบรารี CSV ภายในของ Ruby รวมทั้ง gem ของบุคคลที่สามเช่น `SmarterCSV`, ให้การสนับสนุนที่แข็งแกร่งสำหรับการจัดการข้อมูล CSV, ทำให้สามารถทำงานด้านการประมวลผลและการจัดการข้อมูลได้อย่างมีประสิทธิภาพ.
