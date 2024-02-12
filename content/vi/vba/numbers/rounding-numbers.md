---
title:                "Làm tròn số"
date:                  2024-02-01T22:01:01.707208-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm tròn số"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/vba/rounding-numbers.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Làm gì & Tại sao?

Làm tròn số trong lập trình là việc ước lượng một số về số nguyên gần nhất hoặc đến một số lượng chữ số thập phân nhất định. Các lập trình viên làm tròn số để đơn giản hóa các con số, cải thiện khả năng đọc hoặc đáp ứng các tiêu chí số học cụ thể trong các phép tính, đặc biệt là trong các phép toán tài chính nơi mà độ chính xác là quan trọng.

## Làm thế nào:

Trong Visual Basic cho Ứng dụng (VBA), việc làm tròn có thể đạt được bằng cách sử dụng một số hàm, mỗi hàm phù hợp với các tình huống cụ thể. Dưới đây là một số hàm được sử dụng phổ biến nhất với các ví dụ:

1. **Hàm Round**:
   Hàm `Round` làm tròn một số đến một số chữ số nhất định.
   ```basic
   Dim roundedNumber As Double
   roundedNumber = Round(3.14159, 2)  ' Kết quả: 3.14
   MsgBox roundedNumber
   ```
   
2. **Hàm Int và Fix**:
   Cả hai hàm `Int` và `Fix` đều được sử dụng để làm tròn số xuống số nguyên gần nhất, nhưng chúng có cách hành xử khác nhau với số âm.
   ```basic
   Dim intRounded As Integer
   Dim fixRounded As Integer
   
   intRounded = Int(-3.14159)  ' Kết quả: -4
   fixRounded = Fix(-3.14159)  ' Kết quả: -3
   
   MsgBox "Int: " & intRounded & ", Fix: " & fixRounded
   ```

3. **Hàm Ceiling và Floor**:
   VBA không có sẵn hàm `Ceiling` và `Floor` như trong các ngôn ngữ khác. Để mô phỏng điều này, sử dụng `Application.WorksheetFunction.Ceiling_Math` và `Application.WorksheetFunction.Floor_Math` cho Excel VBA.
   ```basic
   Dim ceilingNumber As Double
   Dim floorNumber As Double
   
   ceilingNumber = Application.WorksheetFunction.Ceiling_Math(3.14159)  ' Kết quả: 4
   floorNumber = Application.WorksheetFunction.Floor_Math(3.14159)  ' Kết quả: 3
   
   MsgBox "Ceiling: " & ceilingNumber & ", Floor: " & floorNumber
   ```

## Đi sâu hơn

Hàm `Round` trong VBA có sự khác biệt cơ bản so với phương pháp làm tròn trong các ngôn ngữ khác do sử dụng **Làm tròn của Ngân hàng**. Làm tròn của Ngân hàng làm tròn đến số chẵn gần nhất khi chính xác ở giữa hai số, giảm thiên vị trong các phép tính trên một tập dữ liệu lớn và cung cấp kết quả có ý nghĩa thống kê cao hơn. Tuy nhiên, điều này có thể dẫn đến hành vi không mong muốn cho những người không quen với nó, đặc biệt là khi mong đợi độ chính xác nguyên số trong mọi trường hợp.

Trái lại, nhiều ngôn ngữ lập trình và hệ thống sử dụng "làm tròn toán học" hoặc "làm tròn nửa lên", nơi mà một số chính xác ở giữa hai giá trị có thể được làm tròn luôn luôn được làm tròn lên. Khi chuyển đổi hoặc chuyển mã từ các ngôn ngữ khác sang VBA, các lập trình viên phải nhớ những khác biệt này để tránh các lỗi tinh tế hoặc không chính xác trong các ứng dụng tài chính và thống kê.

Mặc dù VBA cung cấp nhiều hàm làm tròn, sự thiếu vắng hàm `Ceiling` và `Floor` (không phải sử dụng WorksheetFunction của Excel) làm nổi bật một hạn chế trong khả năng tự nhiên của nó. Các lập trình viên đến từ các ngôn ngữ đa năng hơn có thể tìm thấy những thiếu sót này không tiện lợi và có thể cần phải triển khai các giải pháp tùy chỉnh hoặc điều chỉnh các phép tính của họ để sử dụng các hàm có sẵn. Mặc dù có những hạn chế này, việc hiểu và sử dụng đúng các hàm làm tròn của VBA có thể giúp đảm bảo các phép tính số học chính xác và đáp ứng yêu cầu của hầu hết các ứng dụng.
