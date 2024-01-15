---
title:                "पाठ खोजें और बदलें: एक कंप्यूटर प्रोग्रामिंग आर्टिकल"
html_title:           "C: पाठ खोजें और बदलें: एक कंप्यूटर प्रोग्रामिंग आर्टिकल"
simple_title:         "पाठ खोजें और बदलें: एक कंप्यूटर प्रोग्रामिंग आर्टिकल"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why
Aapne kabhi socha hai ki aap kisi bade saaf-suthre code ko dekhte huye confuse ho jate hai aur usme se kuch specific text ko dhoondhna chahte hai? Ya fir aapko ek bahut bada project mila hai jisme aapko ek particular string ko kai jagah replace karna hai? Aise situations mein "Searching and Replacing" text aapke liye bahut useful ho sakta hai. Isse aap specific text ko quickly dhoondh sakte hai aur usko badi asani se replace kar sakte hai.

## How To
```C
#include <stdio.h>

int main() {
    // Initialize 2 strings - original and replaced
    char *original = "Yeh meri original string hai";
    char *replaced = "Yeh meri replaced string hai";

    // Use String Replace function to replace "original" with "replaced" in a new string
    char *new_string = str_replace(original, "original", "replaced");

    // Print the new string
    printf("New string: %s", new_string);

    // Output: New string: Yeh meri replaced string hai

    return 0;
}

// String Replace Function
// Takes in the original string, text to be replaced and the replacement text
char *str_replace(char *original, char *to_replace, char *replace_with) {
    // Calculate the length of the original string
    int original_length = strlen(original);
    // Calculate the length of the to_replace string
    int to_replace_length = strlen(to_replace);
    // Calculate the length of the replace_with string
    int replace_with_length = strlen(replace_with);

    // Initialize a new string with the same length as the original string
    // This will be the new string with the replaced text
    char *new_string = malloc(original_length);
    // Initialize a position variable for the new string
    int new_string_pos = 0;

    // Loop through the original string
    for(int i=0; i < original_length; i++) {
        // Check if the current position in the string matches the first character of to_replace string
        if(original[i] == to_replace[0]) {
            // Compare the next characters to see if they match the to_replace string
            if(strncmp(&original[i], to_replace, to_replace_length) == 0) {
                // Copy the replace_with string into the new string at the current position
                for(int j=0; j < replace_with_length; j++) {
                    new_string[new_string_pos] = replace_with[j];
                    new_string_pos++;
                }
                // Skip over the characters that were replaced
                i += to_replace_length - 1;
            } else {
                // If the strings don't match, copy the original character into the new string
                new_string[new_string_pos] = original[i];
                new_string_pos++;
            }
        } else {
            // If the characters don't match, copy the original character into the new string
            new_string[new_string_pos] = original[i];
            new_string_pos++;
        }
    }

    // Add a null character at the end to terminate the new string
    new_string[new_string_pos] = '\0';

    // Return the new string
    return new_string;
}
```

## Deep Dive
Searching and Replacing text is a very useful task in programming and can be done in many different ways in C. The example given above is just one of the many possible implementations. Other commonly used methods include using pointers, the `strtok` function and regular expressions. It's important to understand how strings are represented in C and how to manipulate them in order to effectively search and replace text. 

## See Also
- [String Manipulation in C](https://www.programiz.com/c-programming/c-strings)
- [strtok function in C](https://www.geeksforgeeks.org/strtok-strtok_r-functions-c-examples/)
- [Regular expressions in C](https://www.gnu.org/software/libc/manual/html_node/Regular-Expressions.html)