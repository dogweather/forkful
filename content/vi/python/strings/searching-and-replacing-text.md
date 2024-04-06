---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:33.742544-07:00
description: "L\xE0m th\u1EBF n\xE0o: Ng\xE0y x\u01B0a, ch\u1EC9nh s\u1EEDa v\u0103\
  n b\u1EA3n l\xE0 m\u1ED9t qu\xE1 tr\xECnh th\u1EE7 c\xF4ng m\u1EC7t m\u1ECFi. V\xE0\
  o \u0111\u1EA5y, regex (bi\u1EC3u th\u1EE9c ch\xEDnh quy), \u0111\u01B0\u1EE3c x\xE2\
  y d\u1EF1ng trong nh\u1EEFng n\u0103m 1950, \u0111\xE3\u2026"
lastmod: '2024-04-05T21:53:37.509399-06:00'
model: gpt-4-0125-preview
summary: "Ng\xE0y x\u01B0a, ch\u1EC9nh s\u1EEDa v\u0103n b\u1EA3n l\xE0 m\u1ED9t qu\xE1\
  \ tr\xECnh th\u1EE7 c\xF4ng m\u1EC7t m\u1ECFi."
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
