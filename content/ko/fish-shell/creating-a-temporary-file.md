---
title:                "Fish Shell: 임시 파일 생성하기"
simple_title:         "임시 파일 생성하기"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# 왜

컴퓨터 프로그래밍 작업을 할 때 일시적으로 파일을 만들어야 할 때가 있습니다. 이 문제를 해결하는 방법인 임시 파일 생성에 대해 알아보겠습니다.

## 어떻게 해야 할까?

이 글에서는 Fish Shell을 사용하여 임시 파일을 만드는 방법에 대해 알려드리겠습니다. 먼저, "touch" 명령어를 사용해 임시 파일을 만들 수 있습니다. 예를 들어, 다음과 같이 입력해보세요:

```Fish Shell
touch temp_file.txt
```

위의 명령어를 실행하면 "temp_file.txt"라는 이름을 가진 임시 파일이 생성됩니다. 그 다음, "ls" 명령어를 사용하여 파일 목록을 확인할 수 있습니다.

```Fish Shell
ls
```

위 예시에서는 "temp_file.txt"가 목록에 나타납니다. 이제 파일을 사용한 후에는 항상 임시 파일을 삭제해야 합니다. 이를 위해 "rm" 명령어를 사용할 수 있습니다. 다음과 같이 입력해보세요:

```Fish Shell
rm temp_file.txt
```

## 깊게 파고들기

임시 파일을 생성하는 방법은 여러 가지가 있지만, 여기서는 가장 간단한 방법인 "touch" 명령어를 사용하여 설명해드렸습니다. 그러나 임시 파일은 프로그래밍 작업에서 부가적인 기능을 수행하는 것이 아니라면, 적절한 보안 조치를 취하지 않는다면 안전하지 않을 수 있습니다. 따라서 임시 파일을 생성할 때는 꼭 삭제하고, 중요한 정보를 담지 않는 것이 좋습니다.

## 더 알아보기

- [Fish Shell 공식 문서](https://fishshell.com/docs/current/index.html)
- [Fish Shell 튜토리얼](https://fishshell.com/docs/current/tutorial.html)
- [Fish Shell GitHub 저장소](https://github.com/fish-shell/fish-shell)

# 관련 자료

- [고수준 언어에서 임시 파일 생성하기](https://www.ibm.com/support/knowledgecenter/en/SSLTBW_2.2.0/com.ibm.zos.v2r2.ieaa700/cdepr.htm)
- [어서오세요, Linux 임시 파일 생성하기](https://www.tecmint.com/create-temporary-files-in-linux/)
- [임시 파일 관리에 대한 보안 권장 사항](https://www.cisecurity.org/white-papers/temporary-file-management-fall-safety-rules/)