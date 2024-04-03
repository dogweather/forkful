---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:12:18.156107-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: T\u1EA1o v\xE0 s\u1EED d\u1EE5ng m\u1ED9\
  t b\u1EA3n \u0111\u1ED3 trong Kotlin r\u1EA5t \u0111\u01A1n gi\u1EA3n. D\u01B0\u1EDB\
  i \u0111\xE2y l\xE0 h\u01B0\u1EDBng d\u1EABn nhanh v\u1EC1 c\xE1ch l\xE0m."
lastmod: '2024-03-13T22:44:36.592178-06:00'
model: gpt-4-0125-preview
summary: "T\u1EA1o v\xE0 s\u1EED d\u1EE5ng m\u1ED9t b\u1EA3n \u0111\u1ED3 trong Kotlin\
  \ r\u1EA5t \u0111\u01A1n gi\u1EA3n."
title: "S\u1EED d\u1EE5ng m\u1EA3ng li\xEAn k\u1EBFt"
weight: 15
---

## Cách thực hiện:
Tạo và sử dụng một bản đồ trong Kotlin rất đơn giản. Dưới đây là hướng dẫn nhanh về cách làm:

```Kotlin
fun main() {
    // Tạo một bản đồ có thể thay đổi
    val fruits = mutableMapOf("a" to "Táo", "b" to "Chuối")

    // Thêm phần tử
    fruits["o"] = "Cam" // Sử dụng phép chỉ mục
    fruits.put("g", "Nho") // Sử dụng phương thức put

    // Truy cập phần tử
    println(fruits["a"])  // Đầu ra: Táo
    println(fruits["b"])  // Đầu ra: Chuối

    // Xóa phần tử
    fruits.remove("b")
    
    // Lặp qua bản đồ
    for ((key, value) in fruits) {
        println("$key -> $value")
    }
    // Ví dụ đầu ra:
    // a -> Táo
    // o -> Cam
    // g -> Nho
}
```

## Sâu hơn nữa
Bản đồ của Kotlin đến trực tiếp từ khả năng tương tác với Java, nơi mà bản đồ là một phần quan trọng của các bộ sưu tập. Tuy nhiên, Kotlin nâng cao tính hữu ích của chúng bằng cách cung cấp cả giao diện có thể thay đổi (`MutableMap`) và chỉ đọc (`Map`), không giống như giao diện `Map` thống nhất của Java. Sự khác biệt này làm rõ liệu mộc bộ sưu tập có dự định được chỉnh sửa hay không.

Một chi tiết quan trọng về cách thực thi bản đồ của Kotlin là sự phân biệt rõ ràng giữa bản đồ có thể thay đổi và không thể thay đổi, nhấn mạnh tới trọng tâm của ngôn ngữ này vào tính bất biến và an toàn luồng.

Mặc dù bản đồ rất hữu ích, Kotlin cũng cung cấp các bộ sưu tập khác như danh sách và tập hợp, mỗi loại có trường hợp sử dụng riêng. Chẳng hạn, danh sách duy trì thứ tự và cho phép trùng lặp, làm cho chúng lý tưởng để truy cập phần tử theo chỉ mục, trong khi tập hợp đảm bảo tính độc nhất nhưng không duy trì thứ tự. Sự lựa chọn giữa việc sử dụng bản đồ, danh sách, hay tập hợp phụ thuộc vào các yêu cầu cụ thể của ứng dụng của bạn, như nhu cầu truy cập dựa trên khóa hoặc bảo quản thứ tự.

Về các lựa chọn tốt hơn, nếu hiệu suất là điều quan trọng, đặc biệt là với các bộ sưu tập lớn, hãy cân nhắc sử dụng các cấu trúc dữ liệu chuyên biệt, hiệu quả hơn được cung cấp bởi các thư viện bên ngoài được tối ưu hóa cho các trường hợp sử dụng cụ thể, như truy cập đồng thời hoặc sắp xếp.
