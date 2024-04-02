---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:50:11.941330-06:00
description: "\u0E01\u0E32\u0E23\u0E1B\u0E23\u0E31\u0E1A\u0E42\u0E04\u0E23\u0E07\u0E2A\
  \u0E23\u0E49\u0E32\u0E07\u0E42\u0E04\u0E49\u0E14 (Refactoring) \u0E04\u0E37\u0E2D\
  \u0E01\u0E23\u0E30\u0E1A\u0E27\u0E19\u0E01\u0E32\u0E23\u0E02\u0E2D\u0E07\u0E01\u0E32\
  \u0E23\u0E1B\u0E23\u0E31\u0E1A\u0E40\u0E1B\u0E25\u0E35\u0E48\u0E22\u0E19\u0E42\u0E04\
  \u0E49\u0E14\u0E17\u0E35\u0E48\u0E21\u0E35\u0E2D\u0E22\u0E39\u0E48\u0E40\u0E1E\u0E37\
  \u0E48\u0E2D\u0E1B\u0E23\u0E31\u0E1A\u0E1B\u0E23\u0E38\u0E07\u0E42\u0E04\u0E23\u0E07\
  \u0E2A\u0E23\u0E49\u0E32\u0E07, \u0E04\u0E27\u0E32\u0E21\u0E2A\u0E32\u0E21\u0E32\
  \u0E23\u0E16\u0E43\u0E19\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19,\u2026"
lastmod: '2024-03-17T21:57:56.185159-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E1B\u0E23\u0E31\u0E1A\u0E42\u0E04\u0E23\u0E07\u0E2A\
  \u0E23\u0E49\u0E32\u0E07\u0E42\u0E04\u0E49\u0E14 (Refactoring) \u0E04\u0E37\u0E2D\
  \u0E01\u0E23\u0E30\u0E1A\u0E27\u0E19\u0E01\u0E32\u0E23\u0E02\u0E2D\u0E07\u0E01\u0E32\
  \u0E23\u0E1B\u0E23\u0E31\u0E1A\u0E40\u0E1B\u0E25\u0E35\u0E48\u0E22\u0E19\u0E42\u0E04\
  \u0E49\u0E14\u0E17\u0E35\u0E48\u0E21\u0E35\u0E2D\u0E22\u0E39\u0E48\u0E40\u0E1E\u0E37\
  \u0E48\u0E2D\u0E1B\u0E23\u0E31\u0E1A\u0E1B\u0E23\u0E38\u0E07\u0E42\u0E04\u0E23\u0E07\
  \u0E2A\u0E23\u0E49\u0E32\u0E07, \u0E04\u0E27\u0E32\u0E21\u0E2A\u0E32\u0E21\u0E32\
  \u0E23\u0E16\u0E43\u0E19\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19,\u2026"
title: "\u0E01\u0E32\u0E23\u0E1B\u0E23\u0E31\u0E1A\u0E42\u0E04\u0E23\u0E07\u0E2A\u0E23\
  \u0E49\u0E32\u0E07\u0E42\u0E04\u0E49\u0E14"
weight: 19
---

## อะไร & ทำไม?
การปรับโครงสร้างโค้ด (Refactoring) คือกระบวนการของการปรับเปลี่ยนโค้ดที่มีอยู่เพื่อปรับปรุงโครงสร้าง, ความสามารถในการอ่าน, และประสิทธิภาพโดยไม่เปลี่ยนแปลงพฤติกรรมภายนอกของมัน นักโปรแกรมมิ่งทำการปรับโครงสร้างโค้ดเพื่อทำให้โค้ดสามารถดูแลรักษาได้ง่ายขึ้น, เพื่อประโยชน์ในการเพิ่มฟีเจอร์ใหม่ๆ อย่างง่ายดาย, และเพื่อค้นหาและแก้ไขบั๊กได้ง่ายขึ้น

## วิธีการ:
นี่คือตัวอย่างโค้ด Kotlin ที่แสดงถึงกลิ่นโค้ดทั่วไปและเวอร์ชันที่ปรับโครงสร้างแล้ว มาเริ่มกับชิ้นเนื้อโค้ดที่ทำงานมากเกินไป:

```kotlin
fun processOrders(orders: List<Order>) {
    for (order in orders) {
        print("Order ID: ${order.id}")
        // Calculating order total
        var total = 0.0
        for (item in order.items) {
            total += item.price
        }
        // Apply discount
        if (order.customer.isVIP) {
            total *= 0.9
        }
        print("Total: $total")
        // กระบวนการเพิ่มเติม...
    }
}
```

ปรับปรุงเพื่อความสามารถในการอ่านและการแยกส่วนความสนใจที่ดีขึ้น:

```kotlin
fun printOrderSummary(order: Order) {
    print("Order ID: ${order.id}")
    val total = calculateTotal(order)
    print("Total: $total")
}

fun calculateTotal(order: Order): Double {
    var total = order.items.sumOf { it.price }
    return if (order.customer.isVIP) total * 0.9 else total
}

fun processOrders(orders: List<Order>) {
    orders.forEach { printOrderSummary(it) }
}
```

ไม่มีตัวอย่างผลลัพธ์ที่นี่เนื่องจากเราไม่ได้เปลี่ยนแปลงฟังก์ชันการทำงาน, แต่ความสามารถในการอ่านและการดูแลรักษาโค้ดได้รับการปรับปรุงอย่างมาก!

## วิเคราะห์ลึกซึ้ง
การปรับโครงสร้างโค้ดเป็นความคิดที่มีมาตั้งแต่การเริ่มต้นการเขียนโปรแกรม, แต่จริงๆ แล้วได้รับความนิยมเป็นอย่างมากในฐานะวิชาชีพในช่วงทศวรรษที่ 1990, โดยเฉพาะหลังจาก Martin Fowler ได้เผยแพร่หนังสือ "Refactoring: Improving the Design of Existing Code" ในปี 1999 หนังสือเล่มนี้ได้ตั้งชื่อให้กับปฏิบัติการและนิยามวิธีการที่จัดระเบียบสำหรับการใช้งาน รวมถึงแคตตาล็อกของเทคนิคการปรับโครงสร้าง

เมื่อเปรียบเทียบการปรับโครงสร้างกับทางเลือกอื่นๆ: คุณสามารถเขียนโค้ดใหม่จากต้น (มีความเสี่ยงและใช้เวลานาน), หรือเพียงแค่ทำการเปลี่ยนแปลงที่เพิ่มเติม (นำไปสู่การบวมของซอฟต์แวร์และหนี้เทคโนโลยีที่เป็นไปได้) การปรับโครงสร้างเป็นจุดที่เหมาะสม - มันทำให้ทันสมัยและทำความสะอาดขึ้นในขณะที่รักษาระดับความเสี่ยงต่ำ

ในด้านการดำเนินการ, มันสำคัญที่จะต้องมีชุดการทดสอบที่แข็งแกร่งก่อนที่คุณจะเริ่มการปรับโครงสร้างเพื่อรับรองว่าคุณไม่ได้เปลี่ยนแปลงพฤติกรรมของโปรแกรมโดยไม่ได้ตั้งใจ หลาย IDE สมัยใหม่ (รวมถึง IntelliJ สำหรับ Kotlin) มีเครื่องมือการปรับโครงสร้างโค้ดอัตโนมัติเพื่อการเปลี่ยนชื่อตัวแปร, การสร้างเมท็อด, และอื่นๆ, ซึ่งสามารถเร่งกระบวนการและลดข้อผิดพลาด

## ดูเพิ่มเติม
- "Refactoring: Improving the Design of Existing Code" โดย Martin Fowler (สำหรับงานพื้นฐานเกี่ยวกับหัวข้อนี้)
- เอกสารการเขียนโค้ดของ Kotlin: [https://kotlinlang.org/docs/coding-conventions.html](https://kotlinlang.org/docs/coding-conventions.html) (เพื่อเข้าใจวิธีการเขียนโค้ดที่สะอาดแบบ 'Kotlin way')
- การสนับสนุนการปรับโครงสร้างใน IntelliJ IDEA ของ JetBrains: [https://www.jetbrains.com/help/idea/refactoring-source-code.html](https://www.jetbrains.com/help/idea/refactoring-source-code.html) (สำหรับการใช้งานเครื่องมือปรับโครงสร้างโค้ดอย่างปฏิบัติ)
- คู่มือของ Google เกี่ยวกับการปรับโครงสร้างระดับขนาดใหญ่: [https://testing.googleblog.com/2017/06/code-health-to-comment-or-not-to-comment.html](https://testing.googleblog.com/2017/06/code-health-to-comment-or-not-to-comment.html) (สำหรับข้อมูลเชิงลึกเกี่ยวกับการเผชิญกับความท้าทายในการปรับโครงสร้างในระดับใหญ่)
