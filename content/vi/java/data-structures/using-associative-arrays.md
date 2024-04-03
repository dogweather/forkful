---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:12:00.545444-07:00
description: "Trong Java, c\xE1c m\u1EA3ng k\u1EBFt h\u1EE3p, hay b\u1EA3n \u0111\u1ED3\
  , cho ph\xE9p b\u1EA1n l\u01B0u tr\u1EEF c\xE1c c\u1EB7p kh\xF3a-gi\xE1 tr\u1ECB\
  \ \u0111\u1EC3 tra c\u1EE9u v\xE0 thao t\xE1c d\u1EEF li\u1EC7u m\u1ED9t c\xE1ch\
  \ hi\u1EC7u qu\u1EA3. L\u1EADp tr\xECnh vi\xEAn s\u1EED\u2026"
lastmod: '2024-03-13T22:44:36.479831-06:00'
model: gpt-4-0125-preview
summary: "Trong Java, c\xE1c m\u1EA3ng k\u1EBFt h\u1EE3p, hay b\u1EA3n \u0111\u1ED3\
  , cho ph\xE9p b\u1EA1n l\u01B0u tr\u1EEF c\xE1c c\u1EB7p kh\xF3a-gi\xE1 tr\u1ECB\
  \ \u0111\u1EC3 tra c\u1EE9u v\xE0 thao t\xE1c d\u1EEF li\u1EC7u m\u1ED9t c\xE1ch\
  \ hi\u1EC7u qu\u1EA3."
title: "S\u1EED d\u1EE5ng m\u1EA3ng li\xEAn k\u1EBFt"
weight: 15
---

## Gì và Tại sao?

Trong Java, các mảng kết hợp, hay bản đồ, cho phép bạn lưu trữ các cặp khóa-giá trị để tra cứu và thao tác dữ liệu một cách hiệu quả. Lập trình viên sử dụng chúng cho các nhiệm vụ như đếm số lần xuất hiện của các mục hoặc ánh xạ người dùng với quyền của họ bởi vì chúng cung cấp quyền truy cập và cập nhật nhanh chóng.

## Cách thực hiện:

Java không có mảng kết hợp tích hợp sẵn như một số ngôn ngữ khác, nhưng nó cung cấp giao diện `Map` và các lớp như `HashMap` và `TreeMap` để đảm nhận vai trò đó. Dưới đây là cách sử dụng `HashMap`:

```Java
import java.util.HashMap;
import java.util.Map;

public class LearnMaps {
    public static void main(String[] args) {
        // Tạo một HashMap
        Map<String, Integer> tuoiCuaBanBe = new HashMap<>();
        
        // Thêm phần tử
        tuoiCuaBanBe.put("Alice", 24);
        tuoiCuaBanBe.put("Bob", 30);
        tuoiCuaBanBe.put("Charlie", 28);

        // Truy cập phần tử
        System.out.println("Tuổi của Alice: " + tuoiCuaBanBe.get("Alice"));
        
        // Xử lý khóa không tồn tại
        System.out.println("Tuổi của ai đó không có trong bản đồ: " + tuoiCuaBanBe.getOrDefault("Dan", -1));

        // Duyệt qua các phần tử
        for (Map.Entry<String, Integer> entry : tuoiCuaBanBe.entrySet()) {
            System.out.println(entry.getKey() + " " + entry.getValue() + " tuổi.");
        }
    }
}
```

Kết quả mẫu:

```
Tuổi của Alice: 24
Tuổi của ai đó không có trong bản đồ: -1
Alice 24 tuổi.
Bob 30 tuổi.
Charlie 28 tuổi.
```

`HashMap` chỉ là một trong nhiều cài đặt. Nếu khóa của bạn là duy nhất và bạn cần chúng được sắp xếp, hãy cân nhắc `TreeMap`. Đối với một bản đồ giữ nguyên thứ tự chèn, `LinkedHashMap` là lựa chọn dành cho bạn.

## Đi sâu hơn

Bản đồ trong Java là một phần của Framework Collections, được giới thiệu trong JDK 1.2, nhưng đã thấy nhiều cải tiến qua các năm, bao gồm sự giới thiệu của phương thức `forEach` trong Java 8 để dễ dàng duyệt qua các mục nhập hơn. Sự lựa chọn cài đặt bản đồ (`HashMap`, `LinkedHashMap`, `TreeMap`) nên được quyết định bởi nhu cầu cụ thể của bạn về thứ tự và hiệu suất. Ví dụ, `HashMap` cung cấp hiệu suất thời gian O(1) cho các hoạt động cơ bản (get và put), giả định rằng hàm băm phân tán các phần tử một cách đúng đắn giữa các thùng. Tuy nhiên, nếu bạn cần sắp xếp dựa trên thứ tự tự nhiên hoặc so sánh tùy chỉnh, `TreeMap` là lựa chọn hàng đầu, cung cấp thời gian O(log n) cho việc chèn và tra cứu.

Trước khi `Map` được giới thiệu, các mảng kết hợp thường được thực hiện bằng hai mảng song song (một cho khóa, một cho giá trị) hoặc các cấu trúc dữ liệu tùy chỉnh với hiệu suất kém hơn. Các lựa chọn thay thế hiện tại cho `Map` và các cài đặt của nó có thể bao gồm các thư viện bên thứ ba cung cấp các bản đồ chuyên biệt, như bản đồ hai chiều (BiMap trong thư viện Guava của Google) cho các trường hợp bạn cần tìm khóa bằng giá trị của nó một cách hiệu quả. Tuy nhiên, đối với hầu hết các trường hợp sử dụng trong Java, các bản đồ của thư viện chuẩn là rắn chắc và linh hoạt đủ để xử lý nhiệm vụ.
