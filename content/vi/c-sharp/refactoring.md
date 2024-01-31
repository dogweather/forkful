---
title:                "Tái cấu trúc mã"
date:                  2024-01-28T22:06:10.123206-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tái cấu trúc mã"

category:             "C#"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/c-sharp/refactoring.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Tái cấu trúc là quá trình cấu trúc lại mã máy tính hiện có mà không thay đổi hành vi bên ngoài của nó. Các lập trình viên thực hiện điều này để dọn dẹp mã, cải thiện độ rõ ràng, giảm độ phức tạp và cải thiện khả năng bảo trì.

## Làm thế nào:

Hãy tái cấu trúc một phương thức C# đơn giản tính và in tổng của một mảng số:

Trước khi tái cấu trúc:
```C#
public class Calculator
{
    public void CalculateSum()
    {
        int[] numbers = { 1, 2, 3, 4, 5 };
        int sum = 0;
        for (int i = 0; i < numbers.Length; i++)
        {
            sum += numbers[i];
        }
        Console.WriteLine("The sum is " + sum);
    }
}
```

Sau khi tái cấu trúc:
```C#
public class Calculator
{
    private readonly int[] _numbers;

    public Calculator(int[] numbers)
    {
        _numbers = numbers;
    }

    public int CalculateSum()
    {
        return _numbers.Sum();
    }

    public void DisplaySum()
    {
        Console.WriteLine($"The sum is {CalculateSum()}");
    }
}

// Cách sử dụng:
var calculator = new Calculator(new[] { 1, 2, 3, 4, 5 });
calculator.DisplaySum();
```

Bằng cách tái cấu trúc, chúng tôi đã phân chia các mối quan tâm, làm cho lớp `Calculator` linh hoạt hơn bằng cách cho phép nó nhận bất kỳ mảng số nào, và sử dụng LINQ để làm cho việc tính toán tổng trở nên gọn gàng hơn.

## Sâu hơn nữa

Tái cấu trúc có nguồn gốc từ cộng đồng lập trình smalltalk và được phổ biến vào những năm 1990 bởi cuốn sách "Refactoring: Improving the Design of Existing Code" của Martin Fowler. Trải qua nhiều năm, đó đã trở thành một phần cơ bản của các phương pháp linh hoạt và thực hành lập trình tốt.

Có các phương pháp tiếp cận tái cấu trúc khác nhau, như Red-Green-Refactor trong Phát triển Dựa Trên Kiểm Thử (TDD). Nó đảm bảo rằng việc tái cấu trúc không giới thiệu lỗi bằng cách bắt đầu với một bài kiểm tra thất bại, làm cho nó thành công và sau đó là dọn dẹp mã.

Khi thực hiện tái cấu trúc, rất quan trọng phải có một bộ kiểm tra toàn diện để đảm bảo rằng không có chức năng nào bị hỏng trong quá trình. Công cụ tái cấu trúc tự động, như ReSharper cho C#, cũng có thể hỗ trợ trong quá trình này bằng cách cung cấp các cách an toàn để thay đổi các cấu trúc mã. Tuy nhiên, công cụ nên được bổ sung cho một sự hiểu biết sâu sắc về cơ sở mã và nguyên tắc lập trình.

## Xem thêm

- Công trình nền tảng về Tái cấu trúc của Martin Fowler: [Refactoring: Improving the Design of Existing Code](https://martinfowler.com/books/refactoring.html)
- Hướng dẫn của Microsoft về Tái cấu trúc trong Visual Studio: [Tái cấu trúc (C#)](https://docs.microsoft.com/en-us/visualstudio/ide/refactoring-in-visual-studio?view=vs-2022)
- Một cái nhìn chi tiết vào các mẫu tái cấu trúc với ví dụ: [SourceMaking Tái cấu trúc](https://sourcemaking.com/refactoring)
