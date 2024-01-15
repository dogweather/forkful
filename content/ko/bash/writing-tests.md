---
title:                "테스트 작성하기"
html_title:           "Bash: 테스트 작성하기"
simple_title:         "테스트 작성하기"
programming_language: "Bash"
category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/writing-tests.md"
---

{{< edit_this_page >}}

## 왜

테스트 작성을 배우는 이유는 코드의 안정성을 높이고 버그를 사전에 발견할 수 있기 때문입니다.

## 하는 법

```Bash
# 테스트 파일 생성
touch test.sh

# 파이프라인을 사용하여 출력과 테스트 결과를 비교
echo "Hello World!" | ./test.sh

# 테스트 조건을 추가하여 테스트 더욱 강화
if [ $? -eq 0 ]; then
  echo "Test passed!"
else
  echo "Test failed!"
fi
```

## 깊게 파헤치기

- 테스트를 작성하여 코드의 예상되는 결과를 확인할 수 있습니다.
- 시간이 지남에 따라 코드가 변경될 수 있으므로 테스트를 작성하여 코드의 안정성을 유지할 수 있습니다.

## 관련 자료

- [Bash 사용법](https://wiki.kldp.org/HOWTO/html/Adv-Bash-Scr-HOWTO/)
- [코드 품질을 높이는 테스트 작성 방법](https://o365ian.com/how-to-write-test/)