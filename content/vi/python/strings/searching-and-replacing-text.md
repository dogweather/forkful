---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:33.742544-07:00
description: "L\xE0m th\u1EBF n\xE0o: ."
lastmod: '2024-03-13T22:44:36.077148-06:00'
model: gpt-4-0125-preview
summary: .
title: "T\xECm ki\u1EBFm v\xE0 thay th\u1EBF v\u0103n b\u1EA3n"
weight: 10
---

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
