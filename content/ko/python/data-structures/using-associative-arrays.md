---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:12:52.674866-07:00
description: "\uC5F0\uAD00 \uBC30\uC5F4\uC740 Python\uC5D0\uC11C \uC0AC\uC804(dictionary)\uC73C\
  \uB85C \uC54C\uB824\uC838 \uC788\uC73C\uBA70, \uD0A4(keys)\uB97C \uAC12(values)\uC5D0\
  \ \uB9E4\uD551\uD558\uC5EC \uACE0\uC720 \uC2DD\uBCC4\uC790\uB97C \uD1B5\uD55C \uB370\
  \uC774\uD130\uC758 \uAC80\uC0C9, \uC218\uC815 \uB610\uB294 \uCD94\uC801\uC744 \uC6A9\
  \uC774\uD558\uAC8C \uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740\
  \ \uC694\uC18C\uC5D0 \uB300\uD55C \uC811\uADFC\uC758 \uD6A8\uC728\uC131\uACFC \uBCF5\
  \uC7A1\uD55C \uB370\uC774\uD130 \uAD6C\uC870\uB97C \uD45C\uD604\uD558\uB294 \uC720\
  \uC5F0\uC131\u2026"
lastmod: '2024-03-11T00:14:28.501865-06:00'
model: gpt-4-0125-preview
summary: "\uC5F0\uAD00 \uBC30\uC5F4\uC740 Python\uC5D0\uC11C \uC0AC\uC804(dictionary)\uC73C\
  \uB85C \uC54C\uB824\uC838 \uC788\uC73C\uBA70, \uD0A4(keys)\uB97C \uAC12(values)\uC5D0\
  \ \uB9E4\uD551\uD558\uC5EC \uACE0\uC720 \uC2DD\uBCC4\uC790\uB97C \uD1B5\uD55C \uB370\
  \uC774\uD130\uC758 \uAC80\uC0C9, \uC218\uC815 \uB610\uB294 \uCD94\uC801\uC744 \uC6A9\
  \uC774\uD558\uAC8C \uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740\
  \ \uC694\uC18C\uC5D0 \uB300\uD55C \uC811\uADFC\uC758 \uD6A8\uC728\uC131\uACFC \uBCF5\
  \uC7A1\uD55C \uB370\uC774\uD130 \uAD6C\uC870\uB97C \uD45C\uD604\uD558\uB294 \uC720\
  \uC5F0\uC131\u2026"
title: "\uC5F0\uAD00 \uBC30\uC5F4 \uC0AC\uC6A9\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇이며 왜 사용하는가?

연관 배열은 Python에서 사전(dictionary)으로 알려져 있으며, 키(keys)를 값(values)에 매핑하여 고유 식별자를 통한 데이터의 검색, 수정 또는 추적을 용이하게 합니다. 프로그래머들은 요소에 대한 접근의 효율성과 복잡한 데이터 구조를 표현하는 유연성 때문에 이를 사용합니다.

## 방법:

Python에서 사전을 생성하는 것은 간단합니다. 중괄호 `{}` 안에 키-값 쌍을 넣으며, 키와 값은 콜론으로 구분됩니다:

```Python
# 연관 배열(사전) 생성하기
my_dict = {"name": "John", "age": 30, "city": "New York"}
print(my_dict)
```

출력:
```
{'name': 'John', 'age': 30, 'city': 'New York'}
```

키를 통해 값에 접근하는 것은 간단합니다:

```Python
# 값에 접근하기
print(my_dict["name"])
```

출력:
```
John
```

요소를 추가하거나 업데이트하는 것은 키에 값을 할당함으로써 수행됩니다:

```Python
# 새로운 키-값 쌍 추가하기
my_dict["email"] = "john@example.com"
# 값을 업데이트하기
my_dict["age"] = 31
print(my_dict)
```

출력:
```
{'name': 'John', 'age': 31, 'city': 'New York', 'email': 'john@example.com'}
```

사전 항목들을 순회하는 방법:

```Python
# 키-값 쌍을 통해 반복하기
for key, value in my_dict.items():
    print(f"{key}: {value}")
```

출력:
```
name: John
age: 31
city: New York
email: john@example.com
```

## 심층 탐구

Python의 연관 배열 또는 사전은 효율적인 데이터 접근 및 조작을 위한 데이터 구조를 제공하기 위해 도입되었습니다. 수치 범위에 의해 색인되는 시퀀스와는 달리, 사전은 키(key)에 의해 색인되며, 이 키는 변경 불가능한 타입이 될 수 있습니다. 이러한 설계 선택은 키가 고유한 값에 매핑되는 빠른 조회 테이블에 이상적으로 적합합니다.

역사적으로, Python 사전은 해시 테이블을 사용하여 구현되어 평균 조회, 삽입 및 삭제 작업의 시간 복잡도가 O(1)임을 보장했습니다. Python 3.6 이후부터는 사전도 항목의 삽입 순서를 유지하여 해시 테이블의 이점과 순서가 지정된 데이터 구조에서 볼 수 있는 삽입 순서의 예측 가능성을 결합했습니다.

사전은 매우 다재다능하지만, 특별한 경우에는 `collections.defaultdict` 또는 Python 3.7 이전에 `collections.OrderedDict` 같은 대안이 선호될 수 있습니다. `defaultdict`는 존재하지 않는 키에 대해 기본값을 반환해야 할 때 유용하게 사용되며, 특정 유형의 조건부 로직을 단순화합니다. 그러나 Python의 지속적인 개선과 진화로 인해 내장된 사전 클래스는 그 견고함과 바로 사용할 수 있는 편리함 때문에 연관 배열을 위한 가장 주된 선택으로 남아 있습니다.
