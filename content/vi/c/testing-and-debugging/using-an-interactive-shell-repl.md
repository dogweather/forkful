---
aliases:
- /vi/c/using-an-interactive-shell-repl/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:10:34.738001-07:00
description: "M\u1ED9t shell t\u01B0\u01A1ng t\xE1c, c\xF2n \u0111\u01B0\u1EE3c g\u1ECD\
  i l\xE0 V\xF2ng l\u1EB7p \u0110\u1ECDc-\u0110\xE1nh gi\xE1-In (REPL), cho ph\xE9\
  p l\u1EADp tr\xECnh vi\xEAn nh\u1EADp bi\u1EC3u th\u1EE9c ho\u1EB7c m\xE3 v\xE0\
  \ ngay l\u1EADp t\u1EE9c th\u1EA5y k\u1EBFt qu\u1EA3, t\u0103ng\u2026"
lastmod: 2024-02-18 23:08:51.236319
model: gpt-4-0125-preview
summary: "M\u1ED9t shell t\u01B0\u01A1ng t\xE1c, c\xF2n \u0111\u01B0\u1EE3c g\u1ECD\
  i l\xE0 V\xF2ng l\u1EB7p \u0110\u1ECDc-\u0110\xE1nh gi\xE1-In (REPL), cho ph\xE9\
  p l\u1EADp tr\xECnh vi\xEAn nh\u1EADp bi\u1EC3u th\u1EE9c ho\u1EB7c m\xE3 v\xE0\
  \ ngay l\u1EADp t\u1EE9c th\u1EA5y k\u1EBFt qu\u1EA3, t\u0103ng\u2026"
title: "S\u1EED d\u1EE5ng giao di\u1EC7n d\xF2ng l\u1EC7nh t\u01B0\u01A1ng t\xE1c\
  \ (REPL)"
---

{{< edit_this_page >}}

## Cái gì và Tại sao?

Một shell tương tác, còn được gọi là Vòng lặp Đọc-Đánh giá-In (REPL), cho phép lập trình viên nhập biểu thức hoặc mã và ngay lập tức thấy kết quả, tăng cường quá trình học tập và gỡ lỗi. Mặc dù C không truyền thống hỗ trợ môi trường REPL một cách nguyên bản, hiện đại công cụ đang làm cầu nối cho khoảng trống này, cung cấp khám phá động các chương trình C.

## Làm thế nào:

Để tương tác với REPL C, bạn có thể sẽ không tìm thấy một con đường thẳng thắn như trong ngôn ngữ như Python hoặc JavaScript. Tuy nhiên, các công cụ như `Cling`, một bản thông dịch C/C++ dựa trên công nghệ Clang và LLVM, làm cho việc này trở nên khả thi. Dưới đây là cách để bắt đầu:

1. **Cài đặt Cling**: Tùy thuộc vào hệ điều hành của bạn, bạn có thể tìm thấy Cling trong trình quản lý gói của mình hoặc cần phải xây dựng từ mã nguồn. Ví dụ, trên Ubuntu, có thể đơn giản là `sudo apt-get install cling`.

2. **Khởi chạy Cling**: Mở terminal và gõ `cling` để bắt đầu shell tương tác.

```bash
$ cling
```

3. **Viết mã**: Bây giờ bạn có thể nhập mã C trực tiếp vào shell và thấy kết quả ngay lập tức. Dưới đây là một ví dụ đơn giản:

```c
[cling]$ #include <stdio.h>
[cling]$ printf("Xin chào, thế giới REPL!\n");
Xin chào, thế giới REPL!
```

4. **Ví dụ với Biến và Thao tác**: Thử nghiệm với các biến và nhìn thấy phản hồi ngay lập tức.

```c
[cling]$ int a = 5;
[cling]$ int b = 3;
[cling]$ printf("%d + %d = %d\n", a, b, a+b);
5 + 3 = 8
```

5. **Bao gồm Thư viện**: Cling cho phép bạn bao gồm thư viện ngay lập tức, do đó cho phép sử dụng một loạt các chức năng C.

```c
[cling]$ #include <math.h>
[cling]$ printf("Căn bậc hai của %f là %f\n", 4.0, sqrt(4.0));
Căn bậc hai của 4.000000 là 2.000000
```

## Sâu rộng hơn:

Sự ra đời của môi trường REPL có từ Lisp vào những năm 1960, được thiết kế để hỗ trợ việc đánh giá mã một cách tương tác. Tuy nhiên, bản chất tĩnh và biên soạn của C đã tạo ra những thách thức để thực hiện sự ngay lập tức trong điều chỉnh thực thi mã. Sự phát triển của Cling và các bản thông dịch C/C++ khác đánh dấu bước tiến quan trọng hướng tới việc tích hợp đánh giá động vào các ngôn ngữ có kiểu tĩnh.

Đáng chú ý, sử dụng một bản thông dịch như Cling có thể không hoàn hảo phản ánh hành vi của mã C đã biên soạn do sự khác biệt trong tối ưu hoá và thực thi. Ngoài ra, mặc dù rất có giá trị cho mục đích giáo dục, tạo mẫu nhanh và gỡ lỗi, REPL cho C đôi khi có thể chậm hơn và kém thực tiễn cho việc phát triển mã ở cấp độ sản xuất so với chu trình biên soạn-chạy-gỡ lỗi truyền thống.

Các giải pháp thay thế cho lập trình C tương tác bao gồm viết các chương trình nhỏ, tự chứa và sử dụng các IDE mạnh mẽ với các công cụ gỡ lỗi tích hợp, có thể cung cấp nhiều điều khiển và hiểu biết về quá trình thực thi, mặc dù ít ngay lập tức hơn. Mặc dù có những giải pháp thay thế, sự ra đời của môi trường REPL trong C đại diện cho sự mở rộng hấp dẫn của khả năng đa dạng của ngôn ngữ, đáp ứng nhu cầu của kỷ nguyên hiện đại về sự linh hoạt và tốc độ trong các chu trình phát triển.
