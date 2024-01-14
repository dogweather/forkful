---
title:    "Gleam: 난수 생성하기"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## 왜
사람들이 난수를 생성하는 것에 참여하는 이유를 설명하는 1-2 문장.

이 글은 Gleam 프로그래밍에 대한 포스트이며, 난수를 생성하는 방법과 관련된 내용을 다룹니다.

## 방법
아래 예시 코드를 참고하여 난수를 생성하는 방법을 알아보세요.

```Gleam
// 0에서 100까지의 정수를 난수로 생성하는 코드
import random

let random_number = random.number(0, 100)

// 생성된 난수 출력
random_number |> IO.print_line
```

위 코드를 실행하면, 0 이상 100 이하 범위의 난수가 생성되고 해당 숫자가 출력됩니다.

더 많은 정보를 알고 싶다면, Gleam 공식 문서에서 random 라이브러리를 확인해보세요.

## 딥 다이브
난수를 생성하는 알고리즘에 대한 딥 다이브를 진행해보겠습니다.

일반적으로 랜덤 넘버 생성기는 시드(seed)라는 값으로 초기화되며, 시드는 난수의 시작값을 나타냅니다. 따라서 같은 시드값을 넣으면 똑같은 난수가 생성됩니다. 그리고 보통 시드값은 컴퓨터의 현재 시간을 이용해 자동으로 생성됩니다.

그렇기 때문에 컴퓨터의 시간을 바꾸면 같은 시드값이 반복되어 같은 난수가 생성될 수 있습니다. 따라서 더 안정적인 난수 생성을 위해서는 임의로 시드값을 설정하는 것이 좋습니다. Gleam의 random 라이브러리에서는 `new_default_generator()` 함수를 사용하여 임의로 시드값을 설정할 수 있습니다.

더 자세한 내용은 [Gleam 공식 문서](https://gleam.run/docs/stdlib/random)를 참고하세요.

## 참고資料 
- [Gleam 공식 문서](https://gleam.run/)
- [random 라이브러리](https://gleam.run/docs/stdlib/random)