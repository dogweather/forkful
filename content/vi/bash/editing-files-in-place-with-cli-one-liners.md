---
title:                "Chỉnh sửa file tại chỗ với câu lệnh CLI ngắn gọn"
date:                  2024-01-28T22:01:07.081841-07:00
model:                 gpt-4-0125-preview
simple_title:         "Chỉnh sửa file tại chỗ với câu lệnh CLI ngắn gọn"

category:             "Bash"
tag:                  "Data and Text Processing"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/bash/editing-files-in-place-with-cli-one-liners.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì & Tại Sao?

Hãy tưởng tượng bạn vừa phát hiện ra rằng bạn cần phải cập nhật hàng loạt cho một số tệp cấu hình đang nằm trên máy chủ của mình. Bạn có thể mở từng tệp, thực hiện thay đổi bằng tay và lưu chúng. Hoặc, bạn có thể thực hiện chỉnh sửa trực tiếp ngay trên giao diện dòng lệnh (CLI) của mình, một kỹ năng giúp tiết kiệm thời gian, giảm thiểu lỗi và tự động hóa các nhiệm vụ lặp đi lặp lại. Kỹ thuật này đặc biệt hữu ích cho việc cập nhật hệ thống, sửa lỗi, hoặc chỉnh sửa hàng loạt nơi mà việc chỉnh sửa thủ công có thể không khả thi hoặc dễ gặp lỗi.

## Làm Thế Nào:

Khi nói đến việc chỉnh sửa tệp trực tiếp bằng Bash, có hai công cụ nổi bật được sử dụng: `sed` và `awk`. Hãy khám phá cách sử dụng những tiện ích mạnh mẽ này với một số ví dụ về mã.

### Sử dụng `sed` cho việc thay thế văn bản đơn giản

Lệnh sau đây thay thế lần xuất hiện đầu tiên của "text1" bằng "text2" trong `file.txt`:

```Bash
sed -i 's/text1/text2/' file.txt
```

Để thay thế toàn bộ (tất cả các lần xuất hiện), bạn sẽ thêm một `g` ở cuối:

```Bash
sed -i 's/text1/text2/g' file.txt
```

Để chỉnh sửa nhiều tệp cùng một lúc:

```Bash
sed -i 's/text1/text2/g' file1.txt file2.txt file3.txt
```

### Sử dụng `awk` cho các thao tác phức tạp hơn

`awk` là công cụ khác tỏa sáng với khả năng lập trình của mình, đặc biệt hữu ích cho việc xử lý văn bản liên quan đến dữ liệu dựa trên trường.

Thay đổi trường thứ hai của mọi dòng thành `newValue` trong `data.csv`, phân tách bằng dấu phẩy:

```Bash
awk -i inplace -F, '{$2="newValue"; print $0}' OFS=, data.csv
```

### Sao lưu trước khi nhảy

Một lời khuyên hữu ích: luôn tạo bản sao lưu trước khi chỉnh sửa trực tiếp. `sed` hỗ trợ việc này với tùy chọn `-i` theo sau là một hậu tố để tạo bản sao lưu.

```Bash
sed -i.bak 's/text1/text2/g' file.txt
```

Lệnh này tạo một bản sao lưu của `file.txt` gốc với tên là `file.txt.bak` trước khi thực hiện thay thế.

## Sâu Hơn

Khả năng chỉnh sửa tệp trực tiếp từ dòng lệnh xuất hiện như một sự tiếp nối tự nhiên của triết lý Unix: trao quyền cho người dùng quản lý và thao tác dữ liệu một cách hiệu quả với càng ít thao tác nhất có thể. Tuy nhiên, sức mạnh này đi kèm với những hạn chế của nó.

### Bối cảnh lịch sử

Công cụ Unix như `sed` và `awk` đã tồn tại từ những ngày đầu của Unix, được chế tạo như một phần của triết lý công cụ, tập trung vào các lệnh chuyên biệt, có thể kết hợp với nhau. Việc bao gồm chúng vào kho vũ khí của Unix là câu trả lời cho nhu cầu xử lý văn bản hiệu quả trong bối cảnh được thống trị bởi giao diện dòng lệnh.

### Các lựa chọn thay thế

Mặc dù `sed` và `awk` rất mạnh mẽ, nhưng không phải là những lựa chọn duy nhất. Perl và Python, ví dụ, có các tùy chọn dòng lệnh (`-p` và `-i` tương ứng) cho phép khả năng chỉnh sửa tại chỗ tương tự với cú pháp dễ đọc hơn cho các thao tác phức tạp.

```Bash
perl -pi -e 's/text1/text2/g' file.txt
```

```Bash
python -c "import fileinput, sys; [sys.stdout.write(line.replace('text1', 'text2')) for line in fileinput.input(files='file.txt', inplace=True)]"
```

Mỗi lựa chọn đều có điểm mạnh của mình: khả năng viết một dòng lệnh của Perl là rất lớn, và cú pháp của Python có thể dễ tiếp cận hơn đối với những người không sâu sắc về công cụ xử lý văn bản của Unix.

### Chi tiết triển khai

Chỉnh sửa tại chỗ không thực sự "tại chỗ" theo nghĩa kỹ thuật. Cả `sed -i` và `awk -i inplace` đều hoạt động bằng cách tạo một tệp tạm thời nơi đầu ra được xử lý được lưu trữ trước khi thay thế tệp gốc. Cách tiếp cận này đảm bảo rằng tệp không bị hỏng nếu quá trình bị gián đoạn. Hậu quả chủ yếu là về tài nguyên và quyền: bạn phải có đủ không gian đĩa cho tệp tạm thời và quyền tạo tệp trong thư mục của tệp mục tiêu của bạn.

Mặc dù mạnh mẽ, các lệnh chỉnh sửa tại chỗ phải được sử dụng một cách cẩn thận. Một biểu thức chính quy đặt sai có thể dẫn đến mất dữ liệu, nhấn mạnh tầm quan trọng của việc sao lưu. Dù có những rủi ro tiềm ẩn, việc thành thạo những lệnh này có thể cải thiện đáng kể khả năng thực hiện các thay đổi tệp nhanh chóng và hiệu quả trực tiếp từ dòng lệnh, thể hiện triết lý Unix về việc sử dụng các công cụ đơn giản nhưng mạnh mẽ để hoàn thành các nhiệm vụ phức tạp.
