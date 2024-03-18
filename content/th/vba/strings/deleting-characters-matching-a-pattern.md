---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:46:48.296888-06:00
description: "\u0E01\u0E32\u0E23\u0E25\u0E1A\u0E2D\u0E31\u0E01\u0E02\u0E23\u0E30\u0E17\
  \u0E35\u0E48\u0E15\u0E23\u0E07\u0E01\u0E31\u0E1A\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\
  \u0E40\u0E09\u0E1E\u0E32\u0E30\u0E43\u0E19 Visual Basic for Applications (VBA) \u0E40\
  \u0E01\u0E35\u0E48\u0E22\u0E27\u0E02\u0E49\u0E2D\u0E07\u0E01\u0E31\u0E1A\u0E01\u0E32\
  \u0E23\u0E23\u0E30\u0E1A\u0E38\u0E41\u0E25\u0E30\u0E15\u0E48\u0E2D\u0E21\u0E32\u0E04\
  \u0E37\u0E2D\u0E01\u0E32\u0E23\u0E25\u0E1A\u0E2D\u0E31\u0E01\u0E02\u0E23\u0E30\u0E2B\
  \u0E23\u0E37\u0E2D\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E17\u0E35\u0E48\u0E15\u0E2D\u0E1A\
  \u0E2A\u0E19\u0E2D\u0E07\u0E15\u0E48\u0E2D\u0E40\u0E07\u0E37\u0E48\u0E2D\u0E19\u0E44\
  \u0E02\u0E1A\u0E32\u0E07\u0E2D\u0E22\u0E48\u0E32\u0E07\u2026"
lastmod: '2024-03-17T21:57:56.021242-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E25\u0E1A\u0E2D\u0E31\u0E01\u0E02\u0E23\u0E30\u0E17\
  \u0E35\u0E48\u0E15\u0E23\u0E07\u0E01\u0E31\u0E1A\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\
  \u0E40\u0E09\u0E1E\u0E32\u0E30\u0E43\u0E19 Visual Basic for Applications (VBA) \u0E40\
  \u0E01\u0E35\u0E48\u0E22\u0E27\u0E02\u0E49\u0E2D\u0E07\u0E01\u0E31\u0E1A\u0E01\u0E32\
  \u0E23\u0E23\u0E30\u0E1A\u0E38\u0E41\u0E25\u0E30\u0E15\u0E48\u0E2D\u0E21\u0E32\u0E04\
  \u0E37\u0E2D\u0E01\u0E32\u0E23\u0E25\u0E1A\u0E2D\u0E31\u0E01\u0E02\u0E23\u0E30\u0E2B\
  \u0E23\u0E37\u0E2D\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E17\u0E35\u0E48\u0E15\u0E2D\u0E1A\
  \u0E2A\u0E19\u0E2D\u0E07\u0E15\u0E48\u0E2D\u0E40\u0E07\u0E37\u0E48\u0E2D\u0E19\u0E44\
  \u0E02\u0E1A\u0E32\u0E07\u0E2D\u0E22\u0E48\u0E32\u0E07\u2026"
title: "\u0E01\u0E32\u0E23\u0E25\u0E1A\u0E15\u0E31\u0E27\u0E2D\u0E31\u0E01\u0E29\u0E23\
  \u0E17\u0E35\u0E48\u0E15\u0E23\u0E07\u0E01\u0E31\u0E1A\u0E23\u0E39\u0E1B\u0E41\u0E1A\
  \u0E1A"
---

{{< edit_this_page >}}

## อะไร & ทำไม?

การลบอักขระที่ตรงกับรูปแบบเฉพาะใน Visual Basic for Applications (VBA) เกี่ยวข้องกับการระบุและต่อมาคือการลบอักขระหรือสตริงที่ตอบสนองต่อเงื่อนไขบางอย่าง การดำเนินการนี้เป็นสิ่งที่พบบ่อยในงานทำความสะอาดและจัดรูปแบบข้อมูล ที่ซึ่งการลบอักขระที่ไม่จำเป็นหรือไม่ต้องการออกจากสตริงเป็นสิ่งสำคัญเพื่อรักษาความสมบูรณ์ของข้อมูลและส่งเสริมการประมวลผลข้อมูลต่อไป

## วิธีการ:

ใน VBA คุณสามารถใช้ฟังก์ชัน `Replace` หรือ regular expressions ในการลบอักขระที่ตรงกับรูปแบบ ต่อไปนี้คือตัวอย่างของทั้งสองวิธี:

### การใช้ฟังก์ชัน `Replace`

ฟังก์ชัน `Replace` เป็นวิธีที่ตรงไปตรงมาสำหรับการลบอักขระหรือลำดับเฉพาะ

```basic
Sub DeleteSpecificChars()
    Dim originalString As String
    originalString = "123-ABC-456-XYZ"
    
    ' การลบขีดกลาง
    Dim resultString As String
    resultString = Replace(originalString, "-", "")
    
    Debug.Print originalString ' ก่อน: 123-ABC-456-XYZ
    Debug.Print resultString ' หลัง: 123ABC456XYZ
End Sub
```

### การใช้ Regular Expressions

สำหรับรูปแบบที่ซับซ้อนยิ่งขึ้น regular expressions เป็นทางเลือกที่ทรงพลัง

ก่อนอื่น ให้เปิดใช้งาน Microsoft VBScript Regular Expressions library ผ่าน Tools > References ใน Visual Basic Editor


```basic
Sub DeletePatternChars()
    Dim regEx As Object
    Set regEx = CreateObject("VBScript.RegExp")
    
    Dim strPattern As String
    strPattern = "\d" ' รูปแบบเพื่อจับคู่ตัวเลขทั้งหมด
    
    With regEx
        .Global = True
        .IgnoreCase = True
        .Pattern = strPattern
    End With
    
    Dim originalString As String
    originalString = "Remove 123 and 456"
    
    ' การใช้วิธีการ Replace เพื่อลบการจับคู่
    Dim resultString As String
    resultString = regEx.Replace(originalString, "")
    
    Debug.Print originalString ' ก่อน: Remove 123 and 456
    Debug.Print resultString ' หลัง: Remove  and 
End Sub
```

## ลงลึก

ตามประวัติศาสตร์ การจับคู่รูปแบบและการแก้ไขสตริงใน VBA มีความจำกัดอย่างมาก เมื่อเปรียบเทียบกับภาษาโปรแกรมมิ่งสมัยใหม่ที่เสนอไลบรารีมาตรฐานจำนวนมากสำหรับงานเหล่านี้ ฟังก์ชัน `Replace` เรียบง่ายและมีประสิทธิภาพสำหรับการแทนที่โดยตรง แต่ขาดความยืดหยุ่นสำหรับการจับคู่รูปแบบที่ซับซ้อนยิ่งขึ้น นี่คือที่มาของ regular expressions (RegEx) ซึ่งมีไวยากรณ์ที่ซับซ้อนขึ้นสำหรับการจับคู่รูปแบบและการแก้ไขสตริง อย่างไรก็ตาม การทำงานกับ RegEx ใน VBA ต้องการการตั้งค่าเพิ่มเติม เช่น การเปิดใช้งานการอ้างอิง Microsoft VBScript Regular Expressions ซึ่งอาจเป็นอุปสรรคสำหรับผู้ใช้ใหม่

แม้จะมีข้อจำกัดเหล่านี้ การแนะนำการสนับสนุน RegEx ใน VBA เป็นก้าวกระโดดที่สำคัญ ให้เครื่องมือที่ทรงพลังยิ่งขึ้นสำหรับโปรแกรมเมอร์ที่ทำงานกับการประมวลผลข้อความ ในสถานการณ์ที่ซับซ้อนยิ่งขึ้นที่ฟังก์ชันสตริงในตัวล้มเหลว regular expressions ให้ตัวเลือกที่หลากหลายและทรงพลัง

ควรทราบว่า สำหรับผู้ที่ทำงานในสภาพแวดล้อมหรือโครงการที่ประสิทธิภาพเป็นสิ่งสำคัญ การใช้ไลบรารีภายนอกหรือการรวมกับภาษาโปรแกรมมิ่งอื่นอาจให้ประสิทธิภาพที่ดีขึ้นและคุณสมบัติมากขึ้น อย่างไรก็ตาม สำหรับงานประจำวันหลายๆ งานใน VBA เมธอดเหล่านี้ยังคงเป็นตัวเลือกที่ปฏิบัติได้และเข้าถึงได้
