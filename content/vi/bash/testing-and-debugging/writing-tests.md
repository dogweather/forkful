---
title:                "Viết các bài kiểm tra"
aliases:
- /vi/bash/writing-tests/
date:                  2024-01-28T22:13:10.435598-07:00
model:                 gpt-4-0125-preview
simple_title:         "Viết các bài kiểm tra"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/bash/writing-tests.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
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
