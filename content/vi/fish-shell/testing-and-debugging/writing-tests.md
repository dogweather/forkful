---
title:                "Viết các bài kiểm tra"
aliases:
- /vi/fish-shell/writing-tests/
date:                  2024-01-28T22:13:46.216228-07:00
model:                 gpt-4-0125-preview
simple_title:         "Viết các bài kiểm tra"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/fish-shell/writing-tests.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Viết kiểm thử là tạo ra những bài kiểm tra nhỏ để đảm bảo rằng mã của bạn hoạt động như mong đợi. Lập trình viên viết kiểm thử để bắt lỗi sớm, tiết kiệm thời gian và giữ cho mã nguồn đáng tin cậy khi có thay đổi.

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
