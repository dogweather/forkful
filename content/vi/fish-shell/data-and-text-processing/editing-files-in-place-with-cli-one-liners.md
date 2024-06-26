---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:00:55.952857-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Fish Shell, n\u1ED5i ti\u1EBFng v\u1EDB\
  i c\xE1c t\xEDnh n\u0103ng th\xE2n thi\u1EC7n v\u1EDBi ng\u01B0\u1EDDi d\xF9ng v\xE0\
  \ kh\u1EA3 n\u0103ng l\u1EADp tr\xECnh m\u1EA1nh m\u1EBD, cung c\u1EA5p nhi\u1EC1\
  u c\xE1ch \u0111\u1EC3 ch\u1EC9nh s\u1EEDa t\u1EC7p t\u1EA1i\u2026"
lastmod: '2024-03-13T22:44:37.205144-06:00'
model: gpt-4-0125-preview
summary: "Fish Shell, n\u1ED5i ti\u1EBFng v\u1EDBi c\xE1c t\xEDnh n\u0103ng th\xE2\
  n thi\u1EC7n v\u1EDBi ng\u01B0\u1EDDi d\xF9ng v\xE0 kh\u1EA3 n\u0103ng l\u1EADp\
  \ tr\xECnh m\u1EA1nh m\u1EBD, cung c\u1EA5p nhi\u1EC1u c\xE1ch \u0111\u1EC3 ch\u1EC9\
  nh s\u1EEDa t\u1EC7p t\u1EA1i ch\u1ED7."
title: "Ch\u1EC9nh s\u1EEDa file t\u1EA1i ch\u1ED7 v\u1EDBi c\xE2u l\u1EC7nh CLI ng\u1EAF\
  n g\u1ECDn"
weight: 32
---

## Cách thực hiện:
Fish Shell, nổi tiếng với các tính năng thân thiện với người dùng và khả năng lập trình mạnh mẽ, cung cấp nhiều cách để chỉnh sửa tệp tại chỗ. Tuy nhiên, không giống như một số shell khác, Fish không có cơ chế tích hợp sẵn cho việc chỉnh sửa tại chỗ (`sed -i` trong Bash, chẳng hạn). Nhưng đừng lo, bạn vẫn có thể thực hiện điều này với một chút sáng tạo và sự giúp đỡ từ các công cụ bên ngoài như `sed` và `awk`.

### Sử dụng `sed` cho những thay thế đơn giản
Để thay thế tất cả các ví dụ của "hello" bằng "world" trong `file.txt`, bạn sẽ sử dụng:
```Fish Shell
sed -i '' 's/hello/world/g' file.txt
```

### Áp dụng nhiều lệnh `sed`
Nếu bạn cần thực hiện nhiều thay thế, bạn có thể kết nối chúng như thế này:
```Fish Shell
sed -i '' -e 's/fish/bass/g' -e 's/rainbow/trout/g' file.txt
```

### Sử dụng `awk` cho những thao tác phức tạp hơn
Đối với những thao tác quá phức tạp cho `sed`, `awk` có thể là công cụ bạn chọn. Dưới đây là cách nhân đôi số trên mỗi dòng:
```Fish Shell
awk '{print $1 * 2}' file.txt > temp && mv temp file.txt
```

### Lưu ý về Xử lý Lỗi
Hãy nhớ, khi sử dụng những công cụ này từ Fish, việc bắt lỗi và hiểu thông điệp của chúng là rất quan trọng. Sử dụng khả năng xử lý lỗi mạnh mẽ của Fish để làm cho các script của bạn trở nên đáng tin cậy hơn.

## Sâu xa hơn
Trong lịch sử, việc chỉnh sửa tệp tại chỗ đã là một nền tảng của lập trình Unix và Linux, cung cấp một cách hiệu quả để thực hiện các chỉnh sửa nhanh chóng mà không cần phải mở tệp một cách thủ công. Các công cụ như `sed` và `awk` là những tiện ích quý báu đã tồn tại từ những ngày đầu của Unix, trở nên không thể thiếu cho các nhiệm vụ xử lý văn bản.

Fish Shell, dù hiện đại và có những cải tiến về khả năng sử dụng và lập trình, lại thiếu tính năng chỉnh sửa tại chỗ tích hợp sẵn chủ yếu do triết lý thiết kế của nó tập trung vào sự tương tác và thân thiện với người dùng. Sự vắng mặt của một lệnh chỉnh sửa tại chỗ được tích hợp trong Fish nhấn mạnh tầm quan trọng của các công cụ bên ngoài trong hệ sinh thái giống Unix.

Các phương án thay thế cho việc chỉnh sửa tại chỗ trong Fish bao gồm sử dụng tệp tạm thời hoặc tận dụng các dòng lệnh một dòng của Perl hoặc Python, có thể cung cấp nhiều sự linh hoạt hoặc dễ đọc hơn cho các nhiệm vụ phức tạp.

Ví dụ, sử dụng Perl:
```Fish Shell
perl -pi -e 's/find/replace/g' file.txt
```
Hoặc Python:
```Fish Shell
python -c "import re, sys; [sys.stdout.write(re.sub('pattern', 'replacement', line)) for line in sys.stdin]" < file.txt > temp && mv temp file.txt
```

Về mặt thực hiện, khi bạn thực hiện chỉnh sửa tại chỗ, bên dưới lớp vỏ, những công cụ này thường tạo ra một tệp tạm thời, viết các thay đổi vào đó và sau đó thay thế tệp gốc bằng phiên bản đã được chỉnh sửa. Phương pháp này đảm bảo quá trình chỉnh sửa tệp không làm hỏng hoặc mất dữ liệu nếu có lỗi xảy ra trong quá trình hoạt động.

Hiểu biết về những công cụ và phương pháp này cho phép các lập trình viên Fish Shell hiệu quả kết hợp việc chỉnh sửa tại chỗ vào các script của họ, lấp đầy khoảng trống giữa các tính năng thân thiện với người dùng của Fish và sức mạnh thô sơ của các tiện ích xử lý văn bản truyền thống của Unix.
