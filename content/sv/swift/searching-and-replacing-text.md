---
title:    "Swift: Söka och ersätta text"
keywords: ["Swift"]
---

{{< edit_this_page >}}

##VarförDu kanske frågar dig varför man skulle behöva söka och ersätta text i sin Swift-kod. Svaret är enkelt: det är ett effektivt sätt att göra stora ändringar i din kod snabbt och enkelt.

##Hur man gör
Det finns flera olika sätt att hitta och ersätta text i din Swift-kod, men det enklaste sättet är att använda sig av "Find and Replace" funktionen. Detta kan du göra genom att trycka på "Command + F" på ditt tangentbord, eller genom att navigera till "Edit" menyn och välja "Find and Replace". Sedan kan du enkelt skriva in den text du vill hitta och det du vill ersätta den med, och låta Swift göra jobbet åt dig.

```Swift
//Exempel på hur man använder "Find and Replace" i Swift

var myString = "Hej världen!"

//Söker efter "världen" och ersätter det med "Sverige"

myString = myString.replacingOccurrences(of: "världen", with: "Sverige") 

print(myString)

//Output: Hej Sverige!
```

##Djupdykning
Att söka och ersätta text i sin kod kan vara användbart för mer än bara att ändra ord eller fraser. Du kan också använda det för att göra omfattande ändringar i din layout eller för att korrigera stavfel. Det är också möjligt att använda sig av reguljära uttryck för mer avancerad sökning och ersättning.

##Se även
- https://developer.apple.com/documentation/swift/string/replacingoccurrences
- https://www.ios-blog.com/tutorials/swift/how-to-find-and-replace-text-in-swift/