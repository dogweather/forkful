---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:00:05.906152-07:00
description: "Vi\u1EC7c t\xECm \u0111\u1ED9 d\xE0i c\u1EE7a m\u1ED9t chu\u1ED7i trong\
  \ Clojure tr\u1EA3 v\u1EC1 s\u1ED1 l\u01B0\u1EE3ng c\xE1c k\xFD t\u1EF1 trong chu\u1ED7\
  i \u0111\xF3. C\xE1c l\u1EADp tr\xECnh vi\xEAn th\u01B0\u1EDDng c\u1EA7n th\xF4\
  ng tin n\xE0y \u0111\u1EC3 x\xE1c th\u1EF1c \u0111\u1EA7u v\xE0o, l\u1EB7p\u2026"
lastmod: 2024-02-19 22:04:55.337598
model: gpt-4-0125-preview
summary: "Vi\u1EC7c t\xECm \u0111\u1ED9 d\xE0i c\u1EE7a m\u1ED9t chu\u1ED7i trong\
  \ Clojure tr\u1EA3 v\u1EC1 s\u1ED1 l\u01B0\u1EE3ng c\xE1c k\xFD t\u1EF1 trong chu\u1ED7\
  i \u0111\xF3. C\xE1c l\u1EADp tr\xECnh vi\xEAn th\u01B0\u1EDDng c\u1EA7n th\xF4\
  ng tin n\xE0y \u0111\u1EC3 x\xE1c th\u1EF1c \u0111\u1EA7u v\xE0o, l\u1EB7p\u2026"
title: "T\xECm chi\u1EC1u d\xE0i c\u1EE7a m\u1ED9t chu\u1ED7i k\xFD t\u1EF1"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Việc tìm độ dài của một chuỗi trong Clojure trả về số lượng các ký tự trong chuỗi đó. Các lập trình viên thường cần thông tin này để xác thực đầu vào, lặp qua các ký tự, hoặc cho các tác vụ thao tác chuỗi.

## Làm thế nào:
Để lấy độ dài của một chuỗi trong Clojure, sử dụng hàm `count`:

```clojure
(count "Hello, World!") ;=> 13
```

Điều này có nghĩa là "Hello, World!" có 13 ký tự.

## Sâu hơn
Hàm `count` là lựa chọn hàng đầu trong Clojure để tìm số lượng mục trong một bộ sưu tập, và chuỗi không phải là ngoại lệ vì chúng có thể được xem như là một chuỗi các ký tự. Trong lịch sử, `count` đã là một phần của Clojure kể từ các phiên bản đầu tiên, phản ánh rễ của nó trong Lisp nơi các hoạt động về độ dài là phổ biến trên các danh sách.

Một phương án khác cho `count` có thể sử dụng Java interop bởi vì Clojure chạy trên JVM:

```clojure
(.length "Hello, World!") ;=> 13
```

Điều này gọi phương thức `.length` từ lớp String của Java. Mặc dù phương án thay thế này tồn tại, nhưng sử dụng `count` là cách điển hình hơn trong Clojure.

Đáng chú ý là `count` là hoạt động O(1) đối với chuỗi, điều này có nghĩa là nó mất một lượng thời gian cố định bất kể độ dài của chuỗi vì metadata độ dài của chuỗi được lưu trữ.

## Xem thêm
- Tài liệu chính thức của Clojure về `count`: [https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/count](https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/count)
