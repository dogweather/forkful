---
title:    "Bash: 서브스트링 추출하기"
keywords: ["Bash"]
---

{{< edit_this_page >}}

##_ 왜
문자열에서 부분 문자열을 추출하는 방법에 대해 배우는 이유는 여러 가지가 있습니다. 예를 들어, 특정 조건을 만족하는 문자열만을 추출하기 위해 필요할 수 있습니다. 또는 복잡한 문자열에서 원하는 정보만을 추출하여 데이터를 분석하거나 처리하는데 유용할 수 있습니다. 어떤 이유든지, 부분 문자열 추출은 Bash 프로그래밍에서 유용하게 활용될 수 있습니다.

## 어떻게
부분 문자열을 추출하는 방법은 간단합니다. `cut` 명령어를 사용하여 문자열을 잘라내면 됩니다. 다음은 예시 코드와 출력입니다.

```Bash
string="Hello World"
echo ${string:0:5}

# 출력: Hello
```

위의 코드에서 `${string:0:5}`로 지정한 부분은 `string` 변수에서 인덱스 0부터 시작하여 5개의 문자를 추출하는 것을 의미합니다. 이를 통해 원하는 부분 문자열을 효과적으로 추출할 수 있습니다.

## 깊이 파헤치기
부분 문자열 추출에는 더 다양한 방법이 존재합니다. 예를 들어, 특정 패턴을 기준으로 문자열을 나누어 추출할 수 있는 `cut` 명령어의 다양한 옵션이 있습니다. 또는 정규식을 사용하여 더 복잡한 문자열 추출 규칙을 정의할 수도 있습니다. 부분 문자열 추출에 대해 깊이 알아보고 싶다면 `man cut` 혹은 `man regex`와 같은 명령어를 사용하여 더 많은 정보를 확인할 수 있습니다.

## 또 다른 참고자료
- [Bash 쉘 스크립트 투토리얼](https://www.shellscript.sh/tutorial.html)
- [Linux Shell Scripting 튜토리얼](https://www.tutorialspoint.com/unix/shell_scripting.htm)
- [Linux cut 명령어 설명서](https://www.geeksforgeeks.org/cut-command-linux-examples/)
- [Bash 정규식 튜토리얼](https://www.oreilly.com/library/view/mastering-regular-expressions/0596528124/)