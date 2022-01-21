package spell;

import java.util.Objects;

public class Trie implements ITrie{
    private INode root;
    private int numWords;
    private int numNodes;

    public Trie() {
        root = new Node();
        numWords = 0;
        numNodes = 1;
    }

    @Override
    public void add(String word) {
        word = word.toLowerCase();
        INode currentNode = root;
        /**
         * For loop runs through all of the characters in the word,
         * checking to see if nodes have been created.
         * It traverses through the trie on previously created nodes
         * or by creating new ones.
         */
        for (int i = 0; i < word.length(); i++) {
            int nodeIndex = word.charAt(i) - 'a';
            if (currentNode.getChildren()[nodeIndex] == null) {
                currentNode.getChildren()[nodeIndex] = new Node();
                numNodes++;
            }
            currentNode = currentNode.getChildren()[nodeIndex];
        }
        if (currentNode.getValue() == 0) {
            numWords++;
        }
        currentNode.incrementValue();
    }

    @Override
    public INode find(String word) {
        word = word.toLowerCase();
        INode tNode = root;
        for (int i = 0; i < word.length(); i++) {
            int nodeIndex = word.charAt(i) - 'a';
            if (tNode.getChildren()[nodeIndex] == null) {
                return null;
            } else {
                tNode = tNode.getChildren()[nodeIndex];
            }
        }
        if (tNode.getValue() == 0) {
            return null;
        }
        return tNode;
    }

    @Override
    public int getWordCount() {
        return numWords;
    }

    @Override
    public int getNodeCount() {
        return numNodes;
    }

    /**
     * Recursion through the nodes
     */
    public String strRun(INode node, String temp) {
        String words = "";
        if (node.getValue() > 0) {
            words += temp;
            words += "\n";
        }
        for (int i = 0; i < 26; i++) {
            if (node.getChildren()[i] != null) {
                String newTemp = temp + ((char) (i + 'a'));
                words += strRun(node.getChildren()[i], newTemp);
            }
        }
        return words;
    }

    @Override
    public String toString() {
        /**alphabetic list of words*/
        return strRun(root, "");
    }

    @Override
    public boolean equals(Object o) {
        Trie trie = (Trie) o;
        System.out.println(numWords);
        System.out.println(trie.getWordCount());

        if (((Trie) o).numNodes != numNodes) {
            return false;
        }
        if (((Trie) o).numWords != numWords) {
            return false;
        }
        return nEquals(((Trie) o).root, root);
    }

    public boolean nEquals(INode node1, INode node2) {
        boolean equal = true;
        for (int i = 0; i < 26; i++) {
            if (node1.getChildren()[i] == null && node2.getChildren()[i] == null) {
                continue;
            } else if (node1.getChildren()[i] == null || node2.getChildren() == null) {
                return false;
            } else if (node1.getValue() != node2.getValue()) {
                return false;
            }
            equal = nEquals(node1.getChildren()[i], node2.getChildren()[i]);
        }
        return equal;
    }

    @Override
    public int hashCode() {
        int hc = 0;
        for (int i = 0; i < 26; i++) {
            if (root.getChildren()[i] != null) {
                hc += (2^i);
            }
        }
        hc += (getWordCount()*10000);
        hc += (getNodeCount()*10000000);
        return hc;
    }
}
