---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:13:25.384906-07:00
description: "Vi\u1EBFt c\xE1c b\xE0i test ki\u1EC3m tra xem code c\xF3 ho\u1EA1t\
  \ \u0111\u1ED9ng nh\u01B0 mong \u0111\u1EE3i. L\u1EADp tr\xECnh vi\xEAn th\u1EF1\
  c hi\u1EC7n vi\u1EC7c n\xE0y \u0111\u1EC3 ph\xE1t hi\u1EC7n l\u1ED7i s\u1EDBm, b\u1EA3\
  o \u0111\u1EA3m \u0111\u1ED9 tin c\u1EADy v\xE0 d\u1EC5 d\xE0ng thay\u2026"
lastmod: '2024-03-13T22:44:37.345669-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EBFt c\xE1c b\xE0i test ki\u1EC3m tra xem code c\xF3 ho\u1EA1t \u0111\
  \u1ED9ng nh\u01B0 mong \u0111\u1EE3i."
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
