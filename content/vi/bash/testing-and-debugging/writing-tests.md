---
aliases:
- /vi/bash/writing-tests/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:13:10.435598-07:00
description: "Vi\u1EBFt ki\u1EC3m th\u1EED x\xE1c minh r\u1EB1ng m\xE3 l\u1EC7nh ho\u1EA1\
  t \u0111\u1ED9ng nh\u01B0 mong \u0111\u1EE3i. L\u1EADp tr\xECnh vi\xEAn ki\u1EC3\
  m th\u1EED \u0111\u1EC3 b\u1EAFt l\u1ED7i s\u1EDBm, \u0111\u1EA3m b\u1EA3o \u0111\
  \u1ED9 tin c\u1EADy v\xE0 c\u1EADp nh\u1EADt an to\xE0n."
lastmod: 2024-02-18 23:08:50.889506
model: gpt-4-0125-preview
summary: "Vi\u1EBFt ki\u1EC3m th\u1EED x\xE1c minh r\u1EB1ng m\xE3 l\u1EC7nh ho\u1EA1\
  t \u0111\u1ED9ng nh\u01B0 mong \u0111\u1EE3i. L\u1EADp tr\xECnh vi\xEAn ki\u1EC3\
  m th\u1EED \u0111\u1EC3 b\u1EAFt l\u1ED7i s\u1EDBm, \u0111\u1EA3m b\u1EA3o \u0111\
  \u1ED9 tin c\u1EADy v\xE0 c\u1EADp nh\u1EADt an to\xE0n."
title: "Vi\u1EBFt c\xE1c b\xE0i ki\u1EC3m tra"
---

{{< edit_this_page >}}

## Cái gì và Tại sao?

Viết kiểm thử xác minh rằng mã lệnh hoạt động như mong đợi. Lập trình viên kiểm thử để bắt lỗi sớm, đảm bảo độ tin cậy và cập nhật an toàn.

## Làm thế nào:

Bash không có khung kiểm thử có sẵn, nhưng bạn có thể sử dụng các lệnh kiểm tra đơn giản và phép khẳng định. Hãy viết một hàm và kiểm thử nó sử dụng `test` hoặc `[ ]`.

Hàm Bash để kiểm thử:
```Bash
is_number() {
  [[ $1 =~ ^[0-9]+$ ]] && return 0 || return 1
}
```

Trường hợp kiểm thử:
```Bash
test_is_number() {
  is_number 42 && echo "Đạt: 42 là một số" || echo "Không đạt: 42 không phải là một số"
  is_number abc && echo "Đạt: abc là một số" || echo "Không đạt: abc không phải là một số"
}

test_is_number
```

Kết quả mẫu:
```
Đạt: 42 là một số
Không đạt: abc không phải là một số
```

## Sâu hơn

Việc kiểm thử Bash đã phát triển. Ban đầu, người ta sẽ kiểm tra đầu ra của kịch bản một cách thủ công. Sau đó xuất hiện lệnh `test` vào những năm 1970, cho phép kịch bản tự kiểm tra điều kiện. Các lựa chọn khác như `Bats`, một khung kiểm thử Bash thực sự, cung cấp nhiều tính năng hơn nhưng yêu cầu cài đặt bên ngoài. Kết hợp `Bats` với các công cụ Tích hợp Liên tục (CI) như Jenkins hoặc GitHub Actions dẫn tới các quy trình kiểm thử mạnh mẽ hơn. Khi triển khai, nhớ rằng Bash kém chi tiết hơn so với các khung kiểm thử của các ngôn ngữ khác; sử dụng `-eq` để so sánh số, `-z` để kiểm tra xem một chuỗi có rỗng không và `[[ ]]` cho các tính năng nâng cao như so khớp mẫu.

## Xem thêm

- [Bats: Hệ thống Kiểm thử Tự động Bash](https://github.com/bats-core/bats-core)
- [Hướng dẫn Nâng cao Scripting Bash: Kiểm thử và Phân nhánh](https://tldp.org/LDP/abs/html/testbranch.html)
- [ShellCheck: Công cụ phân tích tĩnh cho kịch bản shell](https://www.shellcheck.net/)
