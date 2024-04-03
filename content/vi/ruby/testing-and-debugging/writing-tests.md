---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:13:25.384906-07:00
description: "L\xE0m Th\u1EBF N\xE0o: Ruby s\u1EED d\u1EE5ng Minitest v\xE0 RSpec\
  \ cho vi\u1EC7c test\u2014h\xE3y s\u1EED d\u1EE5ng RSpec. \u0110\u1EA7u ti\xEAn,\
  \ c\xE0i \u0111\u1EB7t n\xF3."
lastmod: '2024-03-13T22:44:37.345669-06:00'
model: gpt-4-0125-preview
summary: "Ruby s\u1EED d\u1EE5ng Minitest v\xE0 RSpec cho vi\u1EC7c test\u2014h\xE3\
  y s\u1EED d\u1EE5ng RSpec."
title: "Vi\u1EBFt c\xE1c b\xE0i ki\u1EC3m tra"
weight: 36
---

## Làm Thế Nào:
Ruby sử dụng Minitest và RSpec cho việc test—hãy sử dụng RSpec. Đầu tiên, cài đặt nó:

```ruby
gem install rspec
```

Tạo một tệp test, `calculator_spec.rb`:

```ruby
RSpec.describe Calculator do
  describe "#add" do
    it "tính tổng hai số" do
      expect(Calculator.new.add(3, 7)).to eql(10)
    end
  end
end
```

Chạy test với:

```shell
rspec calculator_spec.rb
```

Kết quả:

```
F

Các Lỗi:

  1) Calculator#add tính tổng hai số
     Lỗi/Gặp Phải: expect(Calculator.new.add(3, 7)).to eql(10)
     
     NameError:
       hằng số chưa khởi tạo Calculator
```

Tạo `calculator.rb`:

```ruby
class Calculator
  def add(a, b)
    a + b
  end
end
```

Chạy lại các bài test.

Kết quả:

```
.

Hoàn thành trong 0.002 giây (tệp mất 0.08 giây để tải)
1 ví dụ, 0 lỗi
```

## Đào Sâu
Việc test trong Ruby quay trở lại với Test::Unit, nhưng RSpec, giới thiệu năm 2005, đã cách mạng hóa việc test Ruby với "phát triển dựa trên hành vi". Các lựa chọn thay thế cho RSpec bao gồm Minitest và Test::Unit. RSpec tập trung vào khả năng đọc và mặt doanh nghiệp; Minitest minimalist hơn và nhanh hơn. Thông thường, các bài test mô phỏng cách sử dụng phần mềm, kiểm tra các chức năng, dữ liệu và trường hợp rìa. Đối với các dự án đã tồn tại, bắt đầu bằng cách test những phần quan trọng nhất.

## Xem Thêm
- RSpec GitHub: [github.com/rspec/rspec](https://github.com/rspec/rspec)
- Minitest: [rubygems.org/gems/minitest](https://rubygems.org/gems/minitest)
- "Effective Testing with RSpec 3": Đọc thêm về nguyên tắc và mô hình RSpec.
