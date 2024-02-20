---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:41.039120-07:00
description: "L\xE0m vi\u1EC7c v\u1EDBi CSV, vi\u1EBFt t\u1EAFt c\u1EE7a Comma-Separated\
  \ Values (Gi\xE1 tr\u1ECB Ph\xE2n c\xE1ch b\u1EB1ng D\u1EA5u ph\u1EA9y), c\xF3 ngh\u0129\
  a l\xE0 x\u1EED l\xFD d\u1EEF li\u1EC7u d\u01B0\u1EDBi d\u1EA1ng v\u0103n b\u1EA3\
  n thu\u1EA7n t\xFAy, trong \u0111\xF3\u2026"
lastmod: 2024-02-19 22:04:55.683365
model: gpt-4-0125-preview
summary: "L\xE0m vi\u1EC7c v\u1EDBi CSV, vi\u1EBFt t\u1EAFt c\u1EE7a Comma-Separated\
  \ Values (Gi\xE1 tr\u1ECB Ph\xE2n c\xE1ch b\u1EB1ng D\u1EA5u ph\u1EA9y), c\xF3 ngh\u0129\
  a l\xE0 x\u1EED l\xFD d\u1EEF li\u1EC7u d\u01B0\u1EDBi d\u1EA1ng v\u0103n b\u1EA3\
  n thu\u1EA7n t\xFAy, trong \u0111\xF3\u2026"
title: "L\xE0m vi\u1EC7c v\u1EDBi CSV"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Làm việc với CSV, viết tắt của Comma-Separated Values (Giá trị Phân cách bằng Dấu phẩy), có nghĩa là xử lý dữ liệu dưới dạng văn bản thuần túy, trong đó mỗi dòng là một bản ghi dữ liệu với các trường được phân cách bởi dấu phẩy. Lập trình viên đào sâu vào CSV vì chúng đơn giản để sử dụng, được hỗ trợ rộng rãi, và hoàn hảo cho việc trao đổi dữ liệu giữa các ứng dụng khác nhau.

## Cách thức:

Hãy đọc và viết các tệp CSV trong Java sử dụng thư viện `OpenCSV` thường được sử dụng. Đầu tiên, thêm dependency vào `pom.xml` nếu bạn sử dụng Maven.

```xml
<dependency>
    <groupId>com.opencsv</groupId>
    <artifactId>opencsv</artifactId>
    <version>5.6</version> <!-- Kiểm tra phiên bản mới nhất -->
</dependency>
```

### Viết một tệp CSV

```java
import com.opencsv.CSVWriter;
import java.io.FileWriter;
import java.io.IOException;

public class CSVWritingExample {
    public static void main(String[] args) {
        String[] header = {"Name", "Age", "Country"};
        String[] record1 = {"Alice", "24", "USA"};
        String[] record2 = {"Bob", "19", "Canada"};

        try (CSVWriter writer = new CSVWriter(new FileWriter("data.csv"))) {
            writer.writeNext(header);
            writer.writeNext(record1);
            writer.writeNext(record2);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

### Đọc một tệp CSV

```java
import com.opencsv.CSVReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.List;

public class CSVReadingExample {
    public static void main(String[] args) {

        try (CSVReader reader = new CSVReader(new FileReader("data.csv"))) {
            List<String[]> r = reader.readAll();
            r.forEach(x -> System.out.println(x[0] + ", " + x[1] + ", " + x[2]));
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

Kết quả mẫu sau khi đọc:

```
Name, Age, Country
Alice, 24, USA
Bob, 19, Canada
```

## Sâu hơn nữa

Có lịch sử, CSV đã được sử dụng từ những ngày đầu của việc tính toán cá nhân, khiến chúng trở thành một loại lingua franca cho trao đổi dữ liệu. Các lựa chọn khác như JSON, XML, hoặc thậm chí định dạng Excel có thể cung cấp các tính năng tiên tiến hơn, nhưng sự đơn giản của CSV đảm bảo sự lâu dài của nó. Khi làm việc với Java, trong khi `OpenCSV` là một lựa chọn phổ biến, bạn cũng có thể sử dụng `java.util.Scanner` hoặc `java.io.BufferedReader` cho các nhiệm vụ cơ bản, tuy nhiên bạn sẽ tự xử lý phần phân tích cú pháp. `Apache Commons CSV` là một thư viện mạnh mẽ khác có sẵn cho các nhiệm vụ tương tự.

## Xem thêm

- Trang chủ OpenCSV cho tài liệu và hướng dẫn: http://opencsv.sourceforge.net/
- Apache Commons CSV cho một phương pháp tiếp cận khác: https://commons.apache.org/proper/commons-csv/
- Hướng dẫn chính thức của Oracle về các thao tác I/O trong Java: https://docs.oracle.com/javase/tutorial/essential/io/
