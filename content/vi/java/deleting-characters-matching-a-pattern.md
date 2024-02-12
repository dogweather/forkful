---
title:                "Xóa các ký tự phù hợp với một mẫu"
aliases:
- vi/java/deleting-characters-matching-a-pattern.md
date:                  2024-01-28T21:59:24.751243-07:00
model:                 gpt-4-0125-preview
simple_title:         "Xóa các ký tự phù hợp với một mẫu"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/java/deleting-characters-matching-a-pattern.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?
Xóa các ký tự khớp với một mẫu đề cập đến việc tìm các chuỗi ký tự cụ thể trong một chuỗi và loại bỏ chúng. Các lập trình viên làm điều này để dọn dẹp dữ liệu, loại bỏ thông tin không cần thiết, hoặc định dạng chuỗi để phù hợp với một mẫu yêu cầu.

## Cách thực hiện:
Trong Java, chúng ta thường sử dụng phương thức `String.replaceAll()` với một mẫu regex để xóa ký tự. Dưới đây là một ví dụ nhanh:

```Java
public class PatternDeletionExample {
    public static void main(String[] args) {
        String originalString = "Hello, 123 World! This-is a test-string.";
        String pattern = "\\d|-"; // \d là một số, - là dấu gạch ngang

        String cleanedString = originalString.replaceAll(pattern, "");
        System.out.println(cleanedString); // In ra: Hello,  World! This is a teststring.
    }
}
```
Đoạn mã này loại bỏ các chữ số và dấu gạch ngang để làm sạch chuỗi của chúng ta.

## Sâu hơn
Ngày xưa, mọi người thao tác với chuỗi mà không cần đến các phương thức tiện lợi và regex. Họ phải làm việc một cách khó khăn, từng ký tự một, điều này thật đau đớn. Sau đó, biểu thức chính quy (regex) xuất hiện, và mọi thứ trở nên dễ dàng hơn nhiều. Regex là một tiêu chuẩn khớp mẫu mạnh mẽ được sử dụng trong xử lý văn bản.

Vậy tại sao lại sử dụng `replaceAll()`? Nó là một phần của lớp `String` trong Java, và vì chuỗi xuất hiện mọi nơi, nên nó trở thành lựa chọn hàng đầu cho việc chỉnh sửa văn bản dựa trên mẫu. Nó nhận hai tham số: regex cho mẫu cần loại bỏ và thứ để thay thế vào đó—trong trường hợp của chúng ta, một chuỗi rỗng để xóa nó.

Có các phương án thay thế như lớp `Pattern` và `Matcher` cho công việc phức tạp hơn. Những cái này có ích cho các nhiệm vụ tinh tế hơn, như tìm mẫu mà không xóa chúng, hoặc thay thế chúng theo cách phức tạp hơn.

Việc triển khai phụ thuộc vào trình động cơ regex của Java, nó phân tích mẫu và áp dụng nó lên chuỗi mục tiêu. Đó là một nhiệm vụ tìm kiếm và tiêu diệt ký tự mini—tìm mẫu, sau đó loại bỏ nó.

## Xem thêm
- Lớp `Pattern` của Java: [java.util.regex.Pattern](https://docs.oracle.com/javase/10/docs/api/java/util/regex/Pattern.html)
- Lớp `Matcher` của Java: [java.util.regex.Matcher](https://docs.oracle.com/javase/10/docs/api/java/util/regex/Matcher.html)
- Hướng dẫn về Regex: [Biểu thức chính quy - Hướng dẫn sử dụng](https://docs.oracle.com/javase/tutorial/essential/regex/)
- Phương thức `replaceAll()`: [java.lang.String#replaceAll](https://docs.oracle.com/en/java/javase/15/docs/api/java.base/java/lang/String.html#replaceAll(java.lang.String,java.lang.String))
