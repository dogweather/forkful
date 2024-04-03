---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:13:46.216228-07:00
description: "L\xE0m th\u1EBF n\xE0o: Fish kh\xF4ng c\xF3 m\u1ED9t khung ki\u1EC3\
  m th\u1EED t\xEDch h\u1EE3p s\u1EB5n, nh\u01B0ng b\u1EA1n c\xF3 th\u1EC3 d\xF9ng\
  \ `fisher` \u0111\u1EC3 c\xE0i \u0111\u1EB7t m\u1ED9t khung ki\u1EC3m th\u1EED nh\u01B0\
  \ `Fishtape`. D\u01B0\u1EDBi \u0111\xE2y l\xE0 m\u1ED9t\u2026"
lastmod: '2024-03-13T22:44:37.215373-06:00'
model: gpt-4-0125-preview
summary: "Fish kh\xF4ng c\xF3 m\u1ED9t khung ki\u1EC3m th\u1EED t\xEDch h\u1EE3p s\u1EB5\
  n, nh\u01B0ng b\u1EA1n c\xF3 th\u1EC3 d\xF9ng `fisher` \u0111\u1EC3 c\xE0i \u0111\
  \u1EB7t m\u1ED9t khung ki\u1EC3m th\u1EED nh\u01B0 `Fishtape`."
title: "Vi\u1EBFt c\xE1c b\xE0i ki\u1EC3m tra"
weight: 36
---

## Làm thế nào:
Fish không có một khung kiểm thử tích hợp sẵn, nhưng bạn có thể dùng `fisher` để cài đặt một khung kiểm thử như `Fishtape`. Dưới đây là một bài kiểm thử đơn giản với `Fishtape`:

```fish
# Cài đặt Fishtape trước tiên
fisher install jorgebucaran/fishtape

# Tạo một tệp kiểm thử, `test_my_function.fish`
function test_my_function
    echo "Đang chạy kiểm thử my_function"

    # Trường hợp kiểm thử
    my_function argument
    echo $status | fishtape
end

# Chạy tệp kiểm thử của bạn trong Fish Shell
fishtape test_my_function.fish
```

Kết quả mẫu có thể trông như sau:

```
TAP version 13
ok 1 my_function với argument

1..1
# kiểm thử 1
# vượt qua  1

# ok
```

## Đi sâu
Fish shell ra đời vào năm 2005, sau Bash khá lâu. Ngay từ đầu, nó đã tập trung vào các tính năng thông minh và tính dễ sử dụng. Không giống như Bash, nó không đi kèm với các công cụ kiểm thử. Đấy là lúc các công cụ bên thứ ba như `Fishtape` xuất hiện, bổ sung chức năng kiểm thử còn thiếu cho Fish. Hãy nhớ rằng, các kịch bản Fish có thể được kiểm thử như bất kỳ kịch bản nào khác—bằng cách kiểm tra đầu ra và trạng thái thoát—nhưng với `Fishtape`, bạn nhận được đầu ra tuân thủ TAP, dễ sử dụng hơn trong các đường ống CI/CD và với các dụng cụ kiểm thử.

## Xem thêm
Kiểm tra các nguồn lực này để tìm hiểu sâu hơn về Fish Shell và `Fishtape`:
- [Tài liệu Chính thức của Fish](https://fishshell.com/docs/current/index.html)
- [Fishtape trên GitHub](https://github.com/jorgebucaran/fishtape)
- [Quản lý Plugin Fisher](https://github.com/jorgebucaran/fisher)
