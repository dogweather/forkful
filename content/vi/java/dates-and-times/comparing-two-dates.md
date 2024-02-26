---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:57.013409-07:00
description: "So s\xE1nh hai ng\xE0y t\u1EE9c l\xE0 t\xECm ra xem m\u1ED9t ng\xE0\
  y \u0111\u1EBFn tr\u01B0\u1EDBc, sau, hay tr\xF9ng kh\u1EDBp v\u1EDBi ng\xE0y n\xE0\
  o \u0111\xF3. L\u1EADp tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3\
  \ x\u1EED l\xFD l\u1ECBch tr\xECnh, h\u1EA1n ch\xF3t, s\u1EAFp\u2026"
lastmod: '2024-02-25T18:49:34.845427-07:00'
model: gpt-4-0125-preview
summary: "So s\xE1nh hai ng\xE0y t\u1EE9c l\xE0 t\xECm ra xem m\u1ED9t ng\xE0y \u0111\
  \u1EBFn tr\u01B0\u1EDBc, sau, hay tr\xF9ng kh\u1EDBp v\u1EDBi ng\xE0y n\xE0o \u0111\
  \xF3. L\u1EADp tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 x\u1EED\
  \ l\xFD l\u1ECBch tr\xECnh, h\u1EA1n ch\xF3t, s\u1EAFp\u2026"
title: "So s\xE1nh hai ng\xE0y"
---

{{< edit_this_page >}}

## Cái gì và Tại sao?
So sánh hai ngày tức là tìm ra xem một ngày đến trước, sau, hay trùng khớp với ngày nào đó. Lập trình viên làm điều này để xử lý lịch trình, hạn chót, sắp xếp theo thứ tự thời gian, và nhiều hơn nữa.

## Làm thế nào:
Java làm cho việc so sánh ngày tháng trở nên khá dễ dàng. Sử dụng `LocalDate` và các phương thức `compareTo`, `isBefore`, hoặc `isAfter`. Dưới đây là phần mô tả ngắn gọn:

```java
import java.time.LocalDate;

public class DateComparison {
    public static void main(String[] args) {
        LocalDate date1 = LocalDate.of(2023, 4, 1);
        LocalDate date2 = LocalDate.now(); // giả sử hôm nay là 2023-4-15

        // Sử dụng compareTo
        int ketQuaSoSanh = date1.compareTo(date2);
        if (ketQuaSoSanh < 0) {
            System.out.println("Date1 đến trước Date2");
        } else if (ketQuaSoSanh > 0) {
            System.out.println("Date1 đến sau Date2");
        } else {
            System.out.println("Date1 trùng khớp với Date2");
        }

        // Sử dụng isBefore và isAfter
        if (date1.isBefore(date2)) {
            System.out.println("Date1 sớm hơn Date2");
        } else if (date1.isAfter(date2)) {
            System.out.println("Date1 muộn hơn Date2");
        } else {
            System.out.println("Date1 cùng ngày với Date2");
        }
    }
}
```

Kết quả mẫu cho ngày hôm nay là 2023-04-15:

```
Date1 đến trước Date2
Date1 sớm hơn Date2
```

## Sâu hơn
Trước đây, việc xử lý ngày tháng trong Java thực sự là một cơn đau đầu. Nhưng rồi Java 8 đã xuất hiện với `java.time`, một bước tiến lớn. Bây giờ, chúng ta sử dụng `LocalDate` cho các ngày không bao gồm thời gian. Muốn so sánh ngày cả khi có thời gian? Hãy nhìn vào `LocalDateTime`.

Có phương pháp khác không? Chắc chắn rồi. Trước Java 8, đã có `java.util.Date` và `java.util.Calendar`. Bạn vẫn có thể sử dụng chúng, nhưng tại sao lại tự gây khó dễ cho mình?

Về mặt thực hiện, `compareTo` trả về `int`: âm nếu đối tượng gọi nhỏ hơn (đến trước), không nếu bằng, dương nếu lớn hơn (đến sau). `isBefore` và `isAfter` trả về `boolean`. Dễ hiểu và không có bẫy.

## Xem Thêm
Để biết thêm chi tiết, hãy tham khảo những nguồn sau:

- [Tài liệu Java của Oracle về LocalDate](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Hướng dẫn về ngày giờ của Oracle](https://docs.oracle.com/javase/tutorial/datetime/)
- Stack Overflow cho việc sử dụng và khắc phục sự cố trong thực tế:
  - [Sử dụng `LocalDate`](https://stackoverflow.com/questions/tagged/localdate)
  - [So sánh Java Date và Calendar](https://stackoverflow.com/questions/5369682/get-current-time-and-date-on-android)
