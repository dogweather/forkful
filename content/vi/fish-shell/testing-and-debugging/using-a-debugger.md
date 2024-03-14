---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:39.591896-07:00
description: "Vi\u1EC7c s\u1EED d\u1EE5ng debugger ch\u1EE7 y\u1EBFu \u0111\u1EC3\
  \ \"di\u1EC7t bug\" - nh\u1EEFng l\u1ED7i kh\xF3 ch\u1ECBu, h\xFAt m\u1EA5t th\u1EDD\
  i gian trong m\xE3 c\u1EE7a b\u1EA1n. C\xE1c l\u1EADp tr\xECnh vi\xEAn debug v\xEC\
  \ h\u1ECD mu\u1ED1n t\xECm v\xE0 s\u1EEDa c\xE1c\u2026"
lastmod: '2024-03-13T22:44:37.216800-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c s\u1EED d\u1EE5ng debugger ch\u1EE7 y\u1EBFu \u0111\u1EC3 \"di\u1EC7\
  t bug\" - nh\u1EEFng l\u1ED7i kh\xF3 ch\u1ECBu, h\xFAt m\u1EA5t th\u1EDDi gian trong\
  \ m\xE3 c\u1EE7a b\u1EA1n. C\xE1c l\u1EADp tr\xECnh vi\xEAn debug v\xEC h\u1ECD\
  \ mu\u1ED1n t\xECm v\xE0 s\u1EEDa c\xE1c\u2026"
title: "S\u1EED d\u1EE5ng b\u1ED9 g\u1EE1 l\u1ED7i"
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
