---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:54:10.561156-06:00
description: "\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E44\u0E1B\u0E22\u0E31\
  \u0E07 standard error (stderr) \u0E43\u0E19 Fish Shell \u0E04\u0E37\u0E2D\u0E01\u0E32\
  \u0E23\u0E40\u0E1B\u0E25\u0E35\u0E48\u0E22\u0E19\u0E17\u0E34\u0E28\u0E17\u0E32\u0E07\
  \u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E1C\u0E34\u0E14\u0E1E\u0E25\u0E32\u0E14\
  \u0E2B\u0E23\u0E37\u0E2D\u0E01\u0E32\u0E23\u0E27\u0E34\u0E19\u0E34\u0E08\u0E09\u0E31\
  \u0E22\u0E2D\u0E2D\u0E01\u0E08\u0E32\u0E01 standard output (stdout) \u0E41\u0E22\
  \u0E01\u0E15\u0E48\u0E32\u0E07\u0E2B\u0E32\u0E01\u2026"
lastmod: '2024-03-17T21:57:56.660969-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E44\u0E1B\u0E22\u0E31\
  \u0E07 standard error (stderr) \u0E43\u0E19 Fish Shell \u0E04\u0E37\u0E2D\u0E01\u0E32\
  \u0E23\u0E40\u0E1B\u0E25\u0E35\u0E48\u0E22\u0E19\u0E17\u0E34\u0E28\u0E17\u0E32\u0E07\
  \u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E1C\u0E34\u0E14\u0E1E\u0E25\u0E32\u0E14\
  \u0E2B\u0E23\u0E37\u0E2D\u0E01\u0E32\u0E23\u0E27\u0E34\u0E19\u0E34\u0E08\u0E09\u0E31\
  \u0E22\u0E2D\u0E2D\u0E01\u0E08\u0E32\u0E01 standard output (stdout) \u0E41\u0E22\
  \u0E01\u0E15\u0E48\u0E32\u0E07\u0E2B\u0E32\u0E01\u2026"
title: "\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E44\u0E1B\u0E22\u0E31\u0E07\
  \u0E02\u0E49\u0E2D\u0E1C\u0E34\u0E14\u0E1E\u0E25\u0E32\u0E14\u0E21\u0E32\u0E15\u0E23\
  \u0E10\u0E32\u0E19"
---

{{< edit_this_page >}}

## อะไรและทำไม?

การเขียนไปยัง standard error (stderr) ใน Fish Shell คือการเปลี่ยนทิศทางข้อความผิดพลาดหรือการวินิจฉัยออกจาก standard output (stdout) แยกต่างหาก โปรแกรมเมอร์ทำเช่นนี้เพื่อให้แน่ใจว่าสามารถระบุข้อมูลข้อผิดพลาดได้อย่างง่ายดาย จัดการ หรือเปลี่ยนทิศทางได้ ซึ่งช่วยให้กระบวนการดีบักและการบันทึกข้อมูลทำได้ง่ายขึ้น

## วิธีทำ:

ใน Fish Shell, คุณสามารถเขียนไปยัง stderr โดยการเปลี่ยนทิศทางผลลัพธ์ของคุณโดยใช้ `>&2` นี่คือตัวอย่างพื้นฐาน:

```fish
echo "This is an error message" >&2
```

คำสั่งนี้เพียง echo ข้อความไปยัง stderr แทนที่จะเป็น stdout หากคุณจะเขียนสคริปต์ที่ส่งออกข้อความปกติและข้อความผิดพลาด คุณอาจทำอะไรบางอย่างเช่นนี้:

```fish
echo "Starting the process"
echo "An error occurred" >&2
echo "Process completed"
```

ผลลัพธ์ตัวอย่างหากคุณรันสคริปต์และเปลี่ยนทิศทาง stderr ไปยังไฟล์:

```
Starting the process
Process completed
```

ข้อความผิดพลาดจะไม่ปรากฏใน standard output แต่จะพบในไฟล์ที่คุณเปลี่ยนทิศทาง stderr ไป

ในสถานการณ์ที่ต้องการการจัดการข้อผิดพลาดหรือการบันทึกข้อมูลที่ซับซ้อนมากขึ้น Fish Shell ไม่มีไลบรารีในตัวที่ออกแบบมาโดยเฉพาะสำหรับสิ่งนี้ อย่างไรก็ตาม คุณสามารถใช้เครื่องมือภายนอกหรือเขียนฟังก์ชันช่วยได้ ตัวอย่างเช่น การสร้างฟังก์ชันการบันทึกข้อผิดพลาดอย่างง่ายอาจดูเป็นแบบนี้:

```fish
function log_error
    echo $argv >&2
end

log_error "This is an advanced error message"
```

ฟังก์ชัน `log_error` นี้จะรับสตริงใด ๆ ที่คุณให้ไปและเขียนมันไปที่ stderr การใช้ฟังก์ชันเช่นนี้สามารถช่วยให้การจัดการข้อผิดพลาดของคุณสะอาดและสอดคล้องกันทั่วทั้งสคริปต์ของคุณ
