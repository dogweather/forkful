---
aliases:
- /vi/java/using-an-interactive-shell-repl/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:44.621354-07:00
description: "REPL (Read-Eval-Print Loop - V\xF2ng l\u1EB7p \u0110\u1ECDc-\u0110\xE1\
  nh gi\xE1-In) l\xE0 m\u1ED9t shell t\u01B0\u01A1ng t\xE1c x\u1EED l\xFD \u0111\u01A1\
  n l\u1EBB nh\u1EADp t\u1EEB ng\u01B0\u1EDDi d\xF9ng, th\u1EF1c thi m\xE3 l\u1EC7\
  nh v\xE0 tr\u1EA3 v\u1EC1 k\u1EBFt qu\u1EA3. L\u1EADp\u2026"
lastmod: 2024-02-18 23:08:50.559046
model: gpt-4-0125-preview
summary: "REPL (Read-Eval-Print Loop - V\xF2ng l\u1EB7p \u0110\u1ECDc-\u0110\xE1nh\
  \ gi\xE1-In) l\xE0 m\u1ED9t shell t\u01B0\u01A1ng t\xE1c x\u1EED l\xFD \u0111\u01A1\
  n l\u1EBB nh\u1EADp t\u1EEB ng\u01B0\u1EDDi d\xF9ng, th\u1EF1c thi m\xE3 l\u1EC7\
  nh v\xE0 tr\u1EA3 v\u1EC1 k\u1EBFt qu\u1EA3. L\u1EADp\u2026"
title: "S\u1EED d\u1EE5ng v\u1ECF t\u01B0\u01A1ng t\xE1c (REPL)"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
REPL (Read-Eval-Print Loop - Vòng lặp Đọc-Đánh giá-In) là một shell tương tác xử lý đơn lẻ nhập từ người dùng, thực thi mã lệnh và trả về kết quả. Lập trình viên sử dụng nó cho các thử nghiệm nhanh chóng, gỡ lỗi, hoặc học hỏi, vì nó cho phép phản hồi và lặp lại ngay lập tức.

## Làm thế nào:
Bắt đầu một REPL trong Java rất đơn giản với công cụ `jshell` được giới thiệu trong Java 9. Dưới đây là cách để bắt đầu một phiên cơ bản:

```Java
jshell> int sum(int a, int b) {
   ...> return a + b;
   ...> }
|  tạo ra phương thức sum(int,int)

jshell> sum(5, 7)
$1 ==> 12
```

Thoát bất kỳ lúc nào với `/exit`.

```Java
jshell> /exit
|  Tạm biệt
```

## Đào Sâu Hơn
Trước `jshell`, lập trình viên Java không có một REPL chính thức, không giống như những người phát triển Python hay Ruby. Họ sử dụng IDE hoặc viết chương trình đầy đủ ngay cả cho các nhiệm vụ nhỏ nhặt. `jshell` đã làm thay đổi trò chơi từ Java 9, khép lại khoảng cách đó.

Các lựa chọn khác bao gồm trình biên dịch trực tuyến hoặc plugin IDE, nhưng chúng không bì kịp sự trực tiếp của `jshell`. Về nội bộ, `jshell` sử dụng Java Compiler API để thực thi đoạn mã, đó là điều khá tuyệt. Nó hơn là một sân chơi - nó có thể nhập các thư viện, định nghĩa lớp, và nhiều hơn nữa. Điều này làm cho nó trở thành một công cụ mạnh mẽ cho việc tạo mẫu.

## Xem Thêm
- [Hướng dẫn Sử dụng JShell](https://docs.oracle.com/javase/9/jshell/introduction-jshell.htm)
- [Tham khảo Công cụ của Java Platform, Standard Edition](https://docs.oracle.com/javase/9/tools/tools-and-command-reference.htm#JSWOR719)
- [Java Compiler API](https://docs.oracle.com/javase/9/docs/api/javax/tools/JavaCompiler.html)
