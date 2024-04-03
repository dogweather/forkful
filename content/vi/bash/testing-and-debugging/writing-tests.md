---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:13:10.435598-07:00
description: "L\xE0m th\u1EBF n\xE0o: Bash kh\xF4ng c\xF3 khung ki\u1EC3m th\u1EED\
  \ c\xF3 s\u1EB5n, nh\u01B0ng b\u1EA1n c\xF3 th\u1EC3 s\u1EED d\u1EE5ng c\xE1c l\u1EC7\
  nh ki\u1EC3m tra \u0111\u01A1n gi\u1EA3n v\xE0 ph\xE9p kh\u1EB3ng \u0111\u1ECBnh.\
  \ H\xE3y vi\u1EBFt m\u1ED9t h\xE0m v\xE0 ki\u1EC3m th\u1EED n\xF3\u2026"
lastmod: '2024-03-13T22:44:36.883868-06:00'
model: gpt-4-0125-preview
summary: "Bash kh\xF4ng c\xF3 khung ki\u1EC3m th\u1EED c\xF3 s\u1EB5n, nh\u01B0ng\
  \ b\u1EA1n c\xF3 th\u1EC3 s\u1EED d\u1EE5ng c\xE1c l\u1EC7nh ki\u1EC3m tra \u0111\
  \u01A1n gi\u1EA3n v\xE0 ph\xE9p kh\u1EB3ng \u0111\u1ECBnh."
title: "Vi\u1EBFt c\xE1c b\xE0i ki\u1EC3m tra"
weight: 36
---

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
