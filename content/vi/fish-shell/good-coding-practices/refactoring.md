---
title:                "Tái cấu trúc mã"
date:                  2024-01-28T22:06:55.176448-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tái cấu trúc mã"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/fish-shell/refactoring.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì và Tại sao?
Refactoring là quá trình tái cấu trúc mã lệnh hiện tại mà không thay đổi hành vi bên ngoài của nó nhằm cải thiện các thuộc tính phi chức năng. Lập trình viên thực hiện việc này nhằm làm cho mã lệnh dễ đọc hơn, giảm độ phức tạp, cải thiện khả năng bảo trì, và làm cho nó dễ dàng mở rộng hoặc chỉnh sửa sau này.

## Làm thế nào:
Hãy tưởng tượng bạn có một script đã phát triển khá nhiều theo thời gian. Nó bắt đầu một cách đơn giản, nhưng bây giờ nó trở thành một quái vật rộng lớn với những xúc tu lôgic. Đây là một ví dụ nhỏ về việc tái cấu trúc một hàm để nó trở nên dễ đọc và hiệu quả hơn:

Trước khi tái cấu trúc:
```fish
function old_and_clunky
    set color (cat ~/.config/fish/color_theme)
    if test "$color" = 'blue'
        echo 'Đã thiết lập giao diện màu xanh!'
    else if test "$color" = 'red'
        echo 'Đã thiết lập giao diện màu đỏ!'
    else
        echo 'Đã thiết lập giao diện mặc định!'
    end
end
```

Sau khi tái cấu trúc:
```fish
function set_theme_color
    set theme_color (cat ~/.config/fish/color_theme)
    switch $theme_color
        case blue
            echo 'Đã thiết lập giao diện màu xanh!'
        case red
            echo 'Đã thiết lập giao diện màu đỏ!'
        default
            echo 'Đã thiết lập giao diện mặc định!'
    end
end
```
Tái cấu trúc nâng cao tên hàm nhằm mô tả rõ ràng mục đích của nó và thay thế chuỗi if-else bằng một câu lệnh `switch` gọn gàng hơn.

Kết quả mẫu:
```
Đã thiết lập giao diện màu xanh!
```

## Sâu hơn
Refactoring lần đầu được mô tả chi tiết trong cuốn sách tiêu biểu của Martin Fowler "Refactoring: Improving the Design of Existing Code". Cuốn sách đã trình bày một cách có cấu trúc để cải thiện mã lệnh mà không cần viết mới chức năng. Nhiều kỹ thuật tái cấu trúc đã được giới thiệu kể từ đó, và khái niệm này đã trở thành một phần không thể thiếu của phát triển phần mềm hiện đại.

Trong môi trường Fish Shell, việc tái cấu trúc có thể trông hơi khác so với trong các bối cảnh lập trình khác do cú pháp đặc thù và bản chất dòng lệnh của nó. Các phương án thay thế cho việc tái cấu trúc script trong Fish có thể bao gồm việc chuyển sang một ngôn ngữ shell khác hoặc sử dụng công cụ bên ngoài cho việc quản lý script tiên tiến hơn. Tuy nhiên, giữ cú pháp Fish bản địa thường có nghĩa là tích hợp tốt hơn với các tính năng của shell và trải nghiệm tổng thể mượt mà hơn.

Khi tái cấu trúc trong Fish Shell, bạn chủ yếu đang xử lý với các hàm và lệnh thay vì các lớp hoặc module có phạm vi rộng như trong các ngôn ngữ khác. Thông tin granular này có thể làm cho công việc tái cấu trúc trở nên trực tiếp và ngay lập tức hơn, nhưng nó cũng nhấn mạnh tầm quan trọng của mã lệnh rõ ràng, súc tích và dễ bảo trì.

## Xem thêm
- Trang web tái cấu trúc của Martin Fowler: [https://refactoring.com/](https://refactoring.com/)
- Tài liệu chính thức của Fish Shell: [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)
