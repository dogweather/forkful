---
title:                "Tìm kiếm và thay thế văn bản"
aliases:
- /vi/python/searching-and-replacing-text.md
date:                  2024-01-28T22:07:33.742544-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tìm kiếm và thay thế văn bản"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/python/searching-and-replacing-text.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Tìm kiếm và thay thế văn bản là việc tìm kiếm các chuỗi trong một khối văn bản và thay đổi chúng thành cái khác. Lập trình viên làm việc này để chỉnh sửa mã, xử lý dữ liệu, hoặc tự động hóa các nhiệm vụ tái cấu trúc.

## Làm thế nào:
```Python
# Sử dụng str.replace() cho việc thay thế đơn giản
text = "Tôi thích Python. Python thật tuyệt vời!"
text = text.replace("Python", "lập trình")
print(text)  # Đầu ra: Tôi thích lập trình. lập trình thật tuyệt vời!

# Sử dụng re.sub() cho việc thay thế dựa trên mẫu với regex
import re
text = "Liên hệ với chúng tôi qua support@example.com"
new_text = re.sub(r'\b[a-zA-Z0-9.-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}\b', 'support@newdomain.com', text)
print(new_text)  # Đầu ra: Liên hệ với chúng tôi qua support@newdomain.com
```

## Tìm hiểu sâu
Ngày xưa, chỉnh sửa văn bản là một quá trình thủ công mệt mỏi. Vào đấy, regex (biểu thức chính quy), được xây dựng trong những năm 1950, đã làm cho việc tìm kiếm trở nên ít đau đầu hơn. Đối với việc thay thế đơn giản, `str.replace()` là lựa chọn của bạn. Nó đơn giản và tốt cho những thay thế một lần. Khi bạn có các mẫu như số điện thoại, email, hoặc ngày tháng, regex với `re.sub()` là cây đũa thần. Nó tìm kiếm các mẫu với một cú pháp đặc biệt và thay thế chúng. Hãy nhớ, regex có thể quái đản nhưng cũng mạnh mẽ; đó là một công cụ mà bạn càng giải quyết nhiều câu đố thì càng trở nên giỏi hơn.

## Xem thêm
- [Tài liệu Python `str.replace()`](https://docs.python.org/3/library/stdtypes.html#str.replace)
- [Tài liệu mô-đun `re` của Python](https://docs.python.org/3/library/re.html)
- [Regex101](https://regex101.com/): Để thử nghiệm các mẫu regex trực tuyến
- [Automate the Boring Stuff with Python](https://automatetheboringstuff.com/): Một cuốn sách nơi bạn có thể học thêm về các nhiệm vụ xử lý văn bản thực tế.
