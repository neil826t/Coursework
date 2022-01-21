package spell;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.Array;
import java.util.Collections;
import java.util.Scanner;
import java.util.ArrayList;

public class SpellCorrector implements ISpellCorrector{
    private Trie trie;

    public SpellCorrector() {
        trie = new Trie();
    }

    @Override
    public void useDictionary(String dictionaryFileName) throws IOException {
        File file = new File(dictionaryFileName);
        Scanner input = new Scanner(file).useDelimiter("\\s+");
        while(input.hasNext()) {
            String word = input.next();
            trie.add(word);
        }
    }
    public ArrayList<String> similarStrings(ArrayList<String> strings) {
        ArrayList<String> sims = new ArrayList<String>();
        for (String inputWord : strings) {
            //adds the 1 deletion distance strings
            for (int i = 0; i < inputWord.length(); i++) {
                sims.add(inputWord.substring(0, i) + inputWord.substring(i + 1));
            }

            //adds the 1 transposition distance strings
            for (int i = 0; i < inputWord.length() - 1; i++) {
                sims.add(inputWord.substring(0, i) + inputWord.charAt(i + 1)
                        + inputWord.charAt(i) + inputWord.substring(i + 2));
            }

            //adds the 1 alteration distance strings
            for (int i = 0; i < inputWord.length(); i++) {
                for (int ltr = 0; ltr < 26; ltr++) {
                    sims.add(inputWord.substring(0, i)
                            + (char) (ltr + 'a') + inputWord.substring(i + 1));
                }
            }
            //adds the 1 insertion distance strings
            for (int i = 0; i < inputWord.length() + 1; i++) {
                for (int ltr = 0; ltr < 26; ltr++) {
                    sims.add(inputWord.substring(0, i)
                            + (char) (ltr + 'a') + inputWord.substring(i));
                }
            }
        }
        return sims;
    }

    public ArrayList<String> matchingMostCommonStrings(ArrayList<String> strings) {
        //goes through the distance1 and looks at the ones in the dictionary
        //then adds the ones with the highest count to count1
        int maxCount = 1;
        ArrayList<String> count = new ArrayList<String>();
        for (String word1: strings) {
            INode fNode = trie.find(word1);
            if (fNode != null) {
                if (fNode.getValue() == maxCount) {
                    count.add(word1);
                } else if (fNode.getValue() > maxCount) {
                    maxCount = fNode.getValue();
                    count.removeAll(count);
                    count.add(word1);
                }
            }
        }
        return count;
    }

    @Override
    public String suggestSimilarWord(String inputWord) {
        inputWord = inputWord.toLowerCase();
        if (trie.find(inputWord) != null) {
            return inputWord;
        } else {
            ArrayList<String> inputWordAL = new ArrayList<String>();
            inputWordAL.add(inputWord);
            ArrayList<String> distance1 = similarStrings(inputWordAL);
            ArrayList<String> count1 = matchingMostCommonStrings(distance1);
            if (count1.size() == 0) {
                ArrayList<String> distance2 = similarStrings(distance1);
                ArrayList<String> count2 = matchingMostCommonStrings(distance2);
                if (count2.size() == 0) {
                    return null;
                }
                count1 = count2;
            }
            Collections.sort(count1);
            return count1.get(0);
        }
    }
}
