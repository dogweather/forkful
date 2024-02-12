---
title:                "Sử dụng bộ gỡ lỗi"
aliases:
- /vi/fish-shell/using-a-debugger.md
date:                  2024-01-28T22:09:39.591896-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sử dụng bộ gỡ lỗi"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/fish-shell/using-a-debugger.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Việc sử dụng debugger chủ yếu để "diệt bug" - những lỗi khó chịu, hút mất thời gian trong mã của bạn. Các lập trình viên debug vì họ muốn tìm và sửa các vấn đề một cách hiệu quả, hiểu được luồng mã, và có được cái nhìn rõ ràng hơn về những gì mã của họ thực sự đang làm.

## Làm thế nào:
Fish không có debugger tích hợp sẵn như một số shell khác, nhưng bạn có thể sử dụng các công cụ bên ngoài như `gdb` để debug các chương trình đã biên dịch hoặc `fish -d` để chạy fish với đầu ra debug ở các mức độ khác nhau. Chúng ta cùng thử với `fish -d`:

```fish
# Chạy fish shell với mức độ debug 2
fish -d2

# Trong fish shell, chúng ta thử một hàm đơn giản có thể gặp phải lỗi
function test_func
    set val 42
    echo "Giá trị là $val"
    if test $val -eq 42
        echo "Mọi thứ đều ổn."
    else
        echo "Có cái gì đó sai sai."
    end
end

# Gọi hàm và quan sát đầu ra debug
test_func
```

Bạn sẽ thấy thêm đầu ra debug trước và sau khi hàm thực thi, giúp bạn xác định vấn đề.

## Thảo Luận Sâu
Trong lịch sử, việc debug trong môi trường Unix-like đã là lĩnh vực của các công cụ chuyên biệt như `gdb` cho C/C++ hoặc `pdb` cho Python. Trong Fish, bạn thường phụ thuộc vào các tiện ích bên ngoài hoặc các tính năng tích hợp sẵn như `functions -v` cho đầu ra mức độ chi tiết của các hàm và `set -x` để theo dõi các thay đổi biến.

Một số người chọn các shell khác như Bash vì có các tính năng như `set -x` để debug kịch bản. Tuy nhiên, Fish có sức hút riêng với trọng tâm vào sự thân thiện với người dùng và tương tác, có thể giảm bớt nhu cầu debug cấp độ cao trong nhiều trường hợp.

Khi đến phần thực hiện, debug một kịch bản thường liên quan đến việc chạy nó với đầu ra chi tiết và truy vết xem biến được thiết lập, bỏ thiết lập, hoặc thay đổi một cách không mong muốn như thế nào. Với đầu ra màu và cách tiếp cận thân thiện với người dùng của Fish, bạn thường có thể tránh được phần khó nhằn nhất của việc debug - nhưng khi bạn gặp bế tắc, nhớ rằng sự chi tiết và rõ ràng là công cụ tốt nhất của bạn.

## Xem Thêm
Đây là một số phao cứu sinh đáng tin cậy khi bạn đang chìm trong mã:

- Tài liệu Fish về debug: https://fishshell.com/docs/current/index.html#debugging
- Hướng dẫn chính thức của GDB (GNU Debugger): https://www.gnu.org/software/gdb/documentation/
- Thẻ Fish trên Stack Overflow - các trường hợp debug thực tế: https://stackoverflow.com/questions/tagged/fish
- Hướng dẫn nâng cao về Bash-Scripting - để so sánh các phương pháp debug: https://tldp.org/LDP/abs/html/debugging.html
