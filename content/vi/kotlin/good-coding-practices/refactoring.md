---
title:                "Tái cấu trúc mã"
aliases:
- /vi/kotlin/refactoring.md
date:                  2024-01-28T22:07:07.118424-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tái cấu trúc mã"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/kotlin/refactoring.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Tái cấu trúc là quy trình chỉnh sửa mã nguồn hiện có nhằm cải thiện cấu trúc, độ dễ đọc, và hiệu suất của nó mà không thay đổi hành vi bên ngoài của chương trình. Các lập trình viên tiến hành tái cấu trúc để làm cho mã nguồn dễ bảo dưỡng hơn, đơn giản hóa việc thêm các tính năng mới, và dễ dàng phát hiện và sửa lỗi hơn.

## Làm thế nào:
Dưới đây là một đoạn mã Kotlin mô tả một "mùi" mã phổ biến và phiên bản đã được tái cấu trúc của nó. Chúng ta bắt đầu với một khối mã đang làm quá nhiều công việc:

```kotlin
fun processOrders(orders: List<Order>) {
    for (order in orders) {
        print("Mã đơn hàng: ${order.id}")
        // Tính tổng của đơn hàng
        var total = 0.0
        for (item in order.items) {
            total += item.price
        }
        // Áp dụng giảm giá
        if (order.customer.isVIP) {
            total *= 0.9
        }
        print("Tổng cộng: $total")
        // Xử lý thêm...
    }
}
```

Đã được tái cấu trúc để dễ đọc và phân tách mối quan tâm hơn:

```kotlin
fun printOrderSummary(order: Order) {
    print("Mã đơn hàng: ${order.id}")
    val total = calculateTotal(order)
    print("Tổng cộng: $total")
}

fun calculateTotal(order: Order): Double {
    var total = order.items.sumOf { it.price }
    return if (order.customer.isVIP) total * 0.9 else total
}

fun processOrders(orders: List<Order>) {
    orders.forEach { printOrderSummary(it) }
}
```

Không có ví dụ về kết quả ở đây vì chúng tôi không thay đổi chức năng, nhưng độ dễ đọc và khả năng bảo dưỡng của mã nguồn đã được cải thiện rõ rệt!

## Đi sâu hơn
Tái cấu trúc là một khái niệm đã tồn tại từ khi lập trình bắt đầu, nhưng thực sự phát triển mạnh như một lĩnh vực riêng vào những năm 1990, đặc biệt sau khi Martin Fowler xuất bản "Tái Cấu Trúc: Cải Thiện Thiết Kế Của Mã Nguồn Hiện Có" năm 1999. Cuốn sách này đã đặt tên cho thực hành này và định rõ một phương pháp tổ chức để áp dụng nó, bao gồm một danh mục các kỹ thuật tái cấu trúc.

So sánh tái cấu trúc với các phương án thay thế: bạn có thể viết lại mã nguồn từ đầu (rủi ro và tốn thời gian), hoặc chỉ đơn giản là thêm vào các thay đổi (dẫn đến sự phình to của phần mềm và nợ công nghệ tiềm ẩn). Tái cấu trúc đạt được điểm cân bằng lý tưởng - nó hiện đại hóa và dọn dẹp mã nguồn trong khi giữ rủi ro ở mức thấp.

Về mặt thực hiện, điều quan trọng cần có một bộ kiểm tra vững chắc trước khi bạn bắt đầu tái cấu trúc để đảm bảo bạn không vô tình thay đổi hành vi của chương trình. Nhiều môi trường phát triển tích hợp (IDEs) hiện đại (bao gồm IntelliJ cho Kotlin) có các công cụ tái cấu trúc tự động để đổi tên biến, trích xuất phương thức, và nhiều hơn nữa, có thể tăng tốc quá trình này và giảm lỗi.

## Xem thêm
- "Tái Cấu Trúc: Cải Thiện Thiết Kế Của Mã Nguồn Hiện Có" của Martin Fowler (để nắm bắt cơ bản về chủ đề này)
- Tài liệu Kotlin về quy ước mã hóa: [https://kotlinlang.org/docs/coding-conventions.html](https://kotlinlang.org/docs/coding-conventions.html) (để hiểu 'cách của Kotlin' trong việc viết mã sạch)
- Hỗ trợ tái cấu trúc trong IntelliJ IDEA của JetBrains: [https://www.jetbrains.com/help/idea/refactoring-source-code.html](https://www.jetbrains.com/help/idea/refactoring-source-code.html) (để sử dụng công cụ tái cấu trúc thực tế)
- Hướng dẫn của Google về tái cấu trúc trên quy mô lớn: [https://testing.googleblog.com/2017/06/code-health-to-comment-or-not-to-comment.html](https://testing.googleblog.com/2017/06/code-health-to-comment-or-not-to-comment.html) (để tìm hiểu về thách thức khi tái cấu trúc lớn)
